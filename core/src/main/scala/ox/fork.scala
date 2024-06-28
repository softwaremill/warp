package ox

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{CompletableFuture, Semaphore}
import scala.concurrent.ExecutionException
import scala.util.control.NonFatal

/** Implicit evidence that given block of code will be evaluated in a forked scope on a separate thread and therefore that capture of
  * `scala.util.boundary.Label` instances is unsafe.
  */
private[ox] sealed abstract class Forked
private[ox] case object ForkedEvidence extends Forked

/** Starts a fork (logical thread of execution), which is guaranteed to complete before the enclosing [[supervised]] or [[supervisedError]]
  * block completes.
  *
  * The fork behaves as a daemon thread. That is, if the body of the scope completes successfully, and all other user forks (created using
  * [[forkUser]]) complete successfully, the scope will end, cancelling all running forks (including this one, if it's still running). That
  * is, successful completion of this fork isn't required to end the scope.
  *
  * An exception thrown while evaluating `t` will cause the fork to fail and the enclosing scope to end (cancelling all other running
  * forks).
  *
  * For alternate behaviors regarding ending the scope, see [[forkUser]], [[forkError]], [[forkUserError]], [[forkCancellable]] and
  * [[forkUnsupervised]].
  */
def fork[T](f: Forked ?=> T)(using Ox): Fork[T] = forkError(using summon[Ox].asNoErrorMode)(f)

/** Starts a fork (logical thread of execution), which is guaranteed to complete before the enclosing [[supervisedError]] block completes.
  *
  * Behaves the same as [[fork]], but additionally allows reporting application errors represented as values of type `E` in context `F`. An
  * application error causes the enclosing scope to end.
  *
  * Application errors are values with which forks might successfully complete, but are still considered a value-level representation of an
  * error (as opposed to an exception, which isn't a value which is returned, but is thrown instead). Such errors are reported to the
  * enclosing scope. If the [[ErrorMode]] provided when creating the scope using [[supervisedError]] classifies a fork return value as an
  * error, the scope ends (cancelling all other running forks).
  */
def forkError[E, F[_], T](using OxError[E, F])(f: Forked ?=> F[T]): Fork[T] =
  val oxError = summon[OxError[E, F]]
  // the separate result future is needed to wait for the result, as there's no .join on individual tasks (only whole scopes can be joined)
  val result = new CompletableFuture[T]()
  oxError.scope.fork { () =>
    val supervisor = oxError.supervisor
    try
      given Forked = ForkedEvidence
      val resultOrError = f
      val errorMode = oxError.errorMode
      if errorMode.isError(resultOrError) then
        // result is never completed, the supervisor should end the scope
        supervisor.forkAppError(errorMode.getError(resultOrError))
      else result.complete(errorMode.getT(resultOrError))
    catch
      case e: Throwable =>
        // we notify the supervisor first, so that if this is the first failing fork in the scope, the supervisor will
        // get first notified of the exception by the "original" (this) fork
        // if the supervisor doesn't end the scope, the exception will be thrown when joining the result; otherwise, not
        // completing the result; any joins will end up being interrupted
        if !supervisor.forkException(e) then result.completeExceptionally(e).discard
  }
  newForkUsingResult(result)

/** Starts a fork (logical thread of execution), which is guaranteed to complete before the enclosing [[supervised]] or [[supervisedError]]
  * block completes.
  *
  * The fork behaves as a user-level thread. That is, the scope won't end until the body of the scope, and all other user forks (including
  * this one) complete successfully. That is, successful completion of this fork is required to end the scope.
  *
  * An exception thrown while evaluating `t` will cause the enclosing scope to end (cancelling all other running forks).
  *
  * For alternate behaviors, see [[fork]], [[forkError]], [[forkUserError]], [[forkCancellable]] and [[forkUnsupervised]].
  */
def forkUser[T](f: Forked ?=> T)(using Ox): Fork[T] = forkUserError(using summon[Ox].asNoErrorMode)(f)

/** Starts a fork (logical thread of execution), which is guaranteed to complete before the enclosing [[supervisedError]] block completes.
  *
  * Behaves the same as [[forkUser]], but additionally allows reporting application errors represented as values of type `E` in context `F`,
  * which cause the enclosing scope to end.
  *
  * Application errors are values with which forks might successfully complete, but are still considered a value-level representation of an
  * error (as opposed to an exception, which isn't a value which is returned, but is thrown instead). Such errors are reported to the
  * enclosing scope. If the [[ErrorMode]] provided when creating the scope using [[supervisedError]] classifies a fork return value as an
  * error, the scope ends (cancelling all other running forks).
  */
def forkUserError[E, F[_], T](using OxError[E, F])(f: Forked ?=> F[T]): Fork[T] =
  val oxError = summon[OxError[E, F]]
  val result = new CompletableFuture[T]()
  oxError.supervisor.forkStarts()
  oxError.scope.fork { () =>
    val supervisor = oxError.supervisor.asInstanceOf[DefaultSupervisor[E]]
    try
      given Forked = ForkedEvidence
      val resultOrError = f
      val errorMode = oxError.errorMode
      if errorMode.isError(resultOrError) then
        // result is never completed, the supervisor should end the scope
        supervisor.forkAppError(errorMode.getError(resultOrError))
      else
        result.complete(errorMode.getT(resultOrError))
        supervisor.forkSuccess()
    catch
      case e: Throwable =>
        if !supervisor.forkException(e) then result.completeExceptionally(e).discard
  }
  newForkUsingResult(result)

/** Starts a fork (logical thread of execution), which is guaranteed to complete before the enclosing [[supervised]], [[supervisedError]] or
  * [[unsupervised]] block completes.
  *
  * In case an exception is thrown while evaluating `t`, it will be thrown when calling the returned [[Fork]]'s `.join()` method.
  *
  * Success or failure isn't signalled to the enclosing scope, and doesn't influence the scope's lifecycle.
  *
  * For alternate behaviors, see [[fork]], [[forkUser]] and [[forkCancellable]].
  */
def forkUnsupervised[T](f: Forked ?=> T)(using OxUnsupervised): Fork[T] =
  val result = new CompletableFuture[T]()
  summon[OxUnsupervised].scope.fork { () =>
    given Forked = ForkedEvidence
    try result.complete(f)
    catch case e: Throwable => result.completeExceptionally(e)
  }
  newForkUsingResult(result)

/** For each thunk in the given sequence, starts a fork using [[fork]]. All forks are guaranteed to complete before the enclosing
  * [[supervised]] or [[unsupervised]] block completes.
  *
  * If ran in a [[supervised]] scope, all forks behave as daemon threads (see [[fork]] for details).
  */
def forkAll[T](fs: Seq[Forked ?=> () => T])(using Ox): Fork[Seq[T]] =
  val forks = fs.map(f => fork(f()))
  new Fork[Seq[T]]:
    override def join(): Seq[T] = forks.map(_.join())

/** Starts a fork (logical thread of execution), which is guaranteed to complete before the enclosing [[supervised]], [[supervisedError]] or
  * [[unsupervised]] block completes, and which can be cancelled on-demand.
  *
  * In case an exception is thrown while evaluating `t`, it will be thrown when calling the returned [[Fork]]'s `.join()` method.
  *
  * The fork is unsupervised (similarly to [[forkUnsupervised]]), hence success or failure isn't signalled to the enclosing scope and
  * doesn't influence the scope's lifecycle.
  *
  * For alternate behaviors, see [[fork]], [[forkError]], [[forkUser]], [[forkUserError]] and [[forkUnsupervised]].
  *
  * Implementation note: a cancellable fork is created by starting a nested scope in a fork, and then starting a fork there. Hence, it is
  * more expensive than [[fork]], as two virtual threads are started.
  */
def forkCancellable[T](f: Forked ?=> T)(using OxUnsupervised): CancellableFork[T] =
  val result = new CompletableFuture[T]()
  // forks can be never run, if they are cancelled immediately - we need to detect this, not to await on result.get()
  val started = new AtomicBoolean(false)
  // interrupt signal
  val done = new Semaphore(0)
  val ox = summon[OxUnsupervised]
  ox.scope.fork { () =>
    val nestedOx = OxError(NoOpSupervisor, NoErrorMode)
    scopedWithCapability(nestedOx) {
      nestedOx.scope.fork { () =>
        // "else" means that the fork is already cancelled, so doing nothing in that case
        if !started.getAndSet(true) then
          try
            given Forked = ForkedEvidence
            result.complete(f).discard
          catch case e: Throwable => result.completeExceptionally(e).discard

        done.release() // the nested scope can now finish
      }

      done.acquire()
    }
  }
  new CancellableFork[T]:
    override def join(): T = unwrapExecutionException(result.get())

    override def cancel(): Either[Throwable, T] =
      cancelNow()
      try Right(result.get())
      catch
        // we don't want to catch fatal exceptions (excluding IE, which is fatal for the cancelled thread only)
        case e: ExecutionException if e.getCause.isInstanceOf[InterruptedException] => Left(e.getCause)
        case e: ExecutionException if NonFatal.unapply(e.getCause).isDefined        => Left(e.getCause)
        case e: InterruptedException                                                => Left(e)
        case NonFatal(e)                                                            => Left(e)

    override def cancelNow(): Unit =
      // will cause the scope to end, interrupting the task if it hasn't yet finished (or potentially never starting it)
      done.release()
      if !started.getAndSet(true)
      then result.completeExceptionally(new InterruptedException("fork was cancelled before it started")).discard

private def newForkUsingResult[T](result: CompletableFuture[T]): Fork[T] = new Fork[T]:
  override def join(): T = unwrapExecutionException(result.get())

private[ox] inline def unwrapExecutionException[T](f: => T): T =
  try f
  catch
    case e: ExecutionException => throw e.getCause
    case e: Throwable          => throw e

//

/** A fork started using [[fork]], [[forkError]], [[forkUser]], [[forkUserError]], [[forkCancellable]] or [[forkUnsupervised]], backed by a
  * (virtual) thread.
  */
trait Fork[T]:
  /** Blocks until the fork completes with a result.
    *
    * @throws Throwable
    *   If the fork completed with an exception, and is unsupervised (started with [[forkUnsupervised]] or [[forkCancellable]].
    * @see
    *   If `T` is an `Either`, and there's an enclosing [[either]] block, the result can be unwrapped using [[either.ok]].
    */
  def join(): T

  /** Blocks until the fork completes with a result. Only makes sense in for unsupervised forks, that is when the fork is started using
    * [[forkUnsupervised]] or [[forkCancellable]]; otherwise a thrown exception causes the scope to end, and is re-thrown by the
    * [[supervised]] block.
    */
  def joinEither(): Either[Throwable, T] =
    try Right(join())
    catch
      // normally IE is fatal, but here it was meant to cancel the fork, not the joining parent, hence we catch it
      case e: InterruptedException => Left(e)
      case NonFatal(e)             => Left(e)

object Fork:
  /** A dummy pretending to represent a fork which successfully completed with the given value. */
  def successful[T](value: T): Fork[T] = () => value

  /** A dummy pretending to represent a fork which failed with the given exception. */
  def failed[T](e: Throwable): Fork[T] = () => throw e

/** A fork started using [[forkCancellable]], backed by a (virtual) thread. */
trait CancellableFork[T] extends Fork[T]:
  /** Interrupts the fork, and blocks until it completes with a result. */
  def cancel(): Either[Throwable, T]

  /** Interrupts the fork, and returns immediately, without waiting for the fork to complete. Note that the enclosing scope will only
    * complete once all forks have completed.
    */
  def cancelNow(): Unit
