package ox

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap}
import scala.reflect.ClassTag
import scala.util.NotGiven
import scala.util.boundary.Label

sealed abstract class Supervised
object SupervisedEvidence extends Supervised

private inline def availableInScope[A]: Boolean =
  compiletime.summonFrom {
    case _: NotGiven[A] => false
    case _: A           => true
  }

/** Starts a new concurrency scope, which allows starting forks in the given code block `f`. Forks can be started using [[fork]],
  * [[forkUser]], [[forkCancellable]] and [[forkUnsupervised]]. All forks are guaranteed to complete before this scope completes.
  *
  * The scope is ran in supervised mode, that is:
  *   - the scope ends once all user, supervised forks (started using [[forkUser]]), including the `f` body, succeed. Forks started using
  *     [[fork]] (daemon) don't have to complete successfully for the scope to end.
  *   - the scope also ends once the first supervised fork (including the `f` main body) fails with an exception
  *   - when the scope **ends**, all running forks are cancelled
  *   - the scope **completes** (that is, this method returns) only once all forks started by `f` have completed (either successfully, or
  *     with an exception)
  *
  * Upon successful completion, returns the result of evaluating `f`. Upon failure, the exception that caused the scope to end is re-thrown
  * (regardless if the exception was thrown from the main body, or from a fork). Any other exceptions that occur when completing the scope
  * are added as suppressed.
  *
  * @see
  *   [[unsupervised]] Starts a scope in unsupervised mode
  * @see
  *   [[supervisedError]] Starts a scope in supervised mode, with the additional ability to report application errors
  */
inline def supervised[T](f: Supervised ?=> Ox ?=> T): T =
  inline if availableInScope[Label[Either[Nothing, Nothing]]] && availableInScope[Forked] then
    compiletime.error(
      "Nesting supervised scopes along with fork and either blocks is disallowed to prevent unsafe .ok() combinator usage on forks."
    )
  else supervisedErrorInternal(NoErrorMode)(f)

inline def supervisedError[E, F[_], T](em: ErrorMode[E, F])(f: Supervised ?=> OxError[E, F] ?=> F[T]): F[T] =
  inline if availableInScope[Label[Either[Nothing, Nothing]]] && availableInScope[Forked] then
    compiletime.error(
      "Nesting supervised scopes along with fork and either blocks is disallowed to prevent unsafe .ok() combinator usage on forks."
    )
  else supervisedErrorInternal(em)(f)

/** Starts a new concurrency scope, which allows starting forks in the given code block `f`. Forks can be started using [[fork]],
  * [[forkError]], [[forkUser]], [[forkUserError]], [[forkCancellable]] and [[forkUnsupervised]]. All forks are guaranteed to complete
  * before this scope completes.
  *
  * Behaves the same as [[supervised]], but additionally allows reporting application errors represented as values of type `E` in context
  * `F`. An application error causes the enclosing scope to end.
  *
  * @see
  *   [[forkError]] On details how to use application errors.
  */
def supervisedErrorInternal[E, F[_], T](em: ErrorMode[E, F])(f: Supervised ?=> OxError[E, F] ?=> F[T]): F[T] =
  val s = DefaultSupervisor[E]
  val capability = OxError(s, em)
  try
    val scopeResult = scopedWithCapability(capability) {
      val mainBodyFork = forkUserError(using capability)(f(using SupervisedEvidence)(using capability))
      val supervisorResult = s.join() // might throw if any supervised fork threw
      if supervisorResult == ErrorModeSupervisorResult.Success then
        // if no exceptions, the main f-fork must be done by now
        em.pure(mainBodyFork.join())
      else
        // an app error was reported to the supervisor
        em.pureError(supervisorResult.asInstanceOf[E])
    }
    // all forks are guaranteed to have finished; some might have ended up throwing exceptions (InterruptedException or
    // others), but if the scope ended because of an application error, only that result will be returned, hence we have
    // to add the other exceptions as suppressed
    if em.isError(scopeResult) then s.addSuppressedErrors(scopeResult, em) else scopeResult
  catch
    case e: Throwable =>
      // all forks are guaranteed to have finished: some might have ended up throwing exceptions (InterruptedException or
      // others), but only the first one is propagated below. That's why we add all the other exceptions as suppressed.
      s.addSuppressedErrors(e)
      throw e

private[ox] sealed trait Supervisor[-E]:
  def forkStarts(): Unit
  def forkSuccess(): Unit

  /** @return
    *   `true` if the exception was handled by the supervisor, and will cause the scope to end, interrupting any other running forks.
    */
  def forkException(e: Throwable): Boolean
  def forkAppError(e: E): Unit

private[ox] object NoOpSupervisor extends Supervisor[Nothing]:
  override def forkStarts(): Unit = ()
  override def forkSuccess(): Unit = ()
  override def forkException(e: Throwable): Boolean = false
  override def forkAppError(e: Nothing): Unit = ()

private[ox] class DefaultSupervisor[E] extends Supervisor[E]:
  private val running: AtomicInteger = AtomicInteger(0)
  // the result might be completed with: a success marker, an app error, or an exception
  private val result: CompletableFuture[ErrorModeSupervisorResult | E] = new CompletableFuture()
  private val otherExceptions: java.util.Set[Throwable] = ConcurrentHashMap.newKeySet()
  private val otherErrors: java.util.Set[E] = ConcurrentHashMap.newKeySet()

  override def forkStarts(): Unit = running.incrementAndGet().discard

  override def forkSuccess(): Unit =
    val v = running.decrementAndGet()
    if v == 0 then result.complete(ErrorModeSupervisorResult.Success).discard

  override def forkException(e: Throwable): Boolean =
    if !result.completeExceptionally(e) then otherExceptions.add(e).discard
    true

  override def forkAppError(e: E): Unit = if !result.complete(e) then otherErrors.add(e).discard

  /** Wait until the count of all supervised, user forks that are running reaches 0, or until any supervised fork fails with an exception.
    *
    * The completion of this method is typically followed by ending the scope, which cancels any forks that are still running.
    *
    * Note that (daemon) forks can still start supervised user forks after this method returns.
    */
  def join(): ErrorModeSupervisorResult | E = unwrapExecutionException(result.get())

  def addSuppressedErrors(e: Throwable): Throwable =
    otherExceptions.forEach(e2 => if e != e2 then e.addSuppressed(e2))
    otherErrors.forEach(e2 => e.addSuppressed(SecondaryApplicationError(e2)))
    e

  def addSuppressedErrors[F[_], T](r: F[T], errorMode: ErrorMode[E, F]): F[T] =
    var result = r
    otherExceptions.forEach(e => result = errorMode.addSuppressedException(result, e))
    otherErrors.forEach(e => result = errorMode.addSuppressedError(result, e))
    result

private[ox] enum ErrorModeSupervisorResult:
  case Success
