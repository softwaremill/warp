package ox.retry

import ox.{EitherMode, ErrorMode}

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.Try

/** Retries an operation returning a direct result until it succeeds or the policy decides to stop.
  *
  * @param operation
  *   The operation to retry.
  * @param policy
  *   The retry policy - see [[RetryPolicy]].
  * @param lifecycle
  *   The retry lifecycle - see [[RetryLifecycle]]. Defaults to an empty lifecycle.
  * @return
  *   The result of the function if it eventually succeeds.
  * @throws anything
  *   The exception thrown by the last attempt if the policy decides to stop.
  */
def retry[T](operation: => T)(
    policy: RetryPolicy[Throwable, T],
    lifecycle: RetryLifecycle[Throwable, T] = RetryLifecycle.default[Throwable, T]
): T =
  retryEither(Try(operation).toEither)(policy, lifecycle).fold(throw _, identity)

/** Retries an operation returning an [[scala.util.Either]] until it succeeds or the policy decides to stop. Note that any exceptions thrown
  * by the operation aren't caught and don't cause a retry to happen.
  *
  * @param operation
  *   The operation to retry.
  * @param policy
  *   The retry policy - see [[RetryPolicy]].
  * @param lifecycle
  *   The retry lifecycle - see [[RetryLifecycle]]. Defaults to an empty lifecycle.
  * @return
  *   A [[scala.util.Right]] if the function eventually succeeds, or, otherwise, a [[scala.util.Left]] with the error from the last attempt.
  */
def retryEither[E, T](operation: => Either[E, T])(
    policy: RetryPolicy[E, T],
    lifecycle: RetryLifecycle[E, T] = RetryLifecycle.default[E, T]
): Either[E, T] =
  retry(EitherMode[E])(operation)(policy, lifecycle)

/** Retries an operation using the given error mode until it succeeds or the policy decides to stop. Note that any exceptions thrown by the
  * operation aren't caught (unless the operation catches them as part of its implementation) and don't cause a retry to happen.
  *
  * @param em
  *   The error mode to use, which specifies when a result value is considered success, and when a failure.
  * @param operation
  *   The operation to retry.
  * @param policy
  *   The retry policy - see [[RetryPolicy]].
  * @param lifecycle
  *   The retry lifecycle - see [[RetryLifecycle]].
  * @return
  *   Either:
  *   - the result of the function if it eventually succeeds, in the context of `F`, as dictated by the error mode.
  *   - the error `E` in context `F` as returned by the last attempt if the policy decides to stop.
  */
def retry[E, F[_], T](em: ErrorMode[E, F])(operation: => F[T])(policy: RetryPolicy[E, T], lifecycle: RetryLifecycle[E, T]): F[T] =
  @tailrec
  def loop(attempt: Int, remainingAttempts: Option[Int], lastDelay: Option[FiniteDuration]): F[T] =
    def sleepIfNeeded =
      val delay = policy.schedule.nextDelay(attempt + 1, lastDelay).toMillis
      if (delay > 0) Thread.sleep(delay)
      delay

    lifecycle.beforeEachAttempt(attempt + 1)

    operation match
      case v if em.isError(v) =>
        val error = em.getError(v)
        lifecycle.afterEachAttempt(attempt + 1, Left(error))

        if policy.resultPolicy.isWorthRetrying(error) && remainingAttempts.forall(_ > 0) then
          val delay = sleepIfNeeded
          loop(attempt + 1, remainingAttempts.map(_ - 1), Some(delay.millis))
        else v
      case v =>
        val result = em.getT(v)
        lifecycle.afterEachAttempt(attempt + 1, Right(result))

        if !policy.resultPolicy.isSuccess(result) && remainingAttempts.forall(_ > 0) then
          val delay = sleepIfNeeded
          loop(attempt + 1, remainingAttempts.map(_ - 1), Some(delay.millis))
        else v

  val remainingAttempts = policy.schedule match
    case finiteSchedule: Schedule.Finite => Some(finiteSchedule.maxRetries)
    case _                               => None

  loop(0, remainingAttempts, None)
