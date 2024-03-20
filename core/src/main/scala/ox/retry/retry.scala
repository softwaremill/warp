package ox.retry

import ox.{EitherMode, ErrorMode}

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.Try

/** Retries an operation returning a direct result until it succeeds or the policy decides to stop.
  *
  * @param operation
  *   The operation to retry.
  * @param onRetry
  *   The function to call on each retry - see [[OnRetry]].
  * @param policy
  *   The retry policy - see [[RetryPolicy]].
  * @return
  *   The result of the function if it eventually succeeds.
  * @throws anything
  *   The exception thrown by the last attempt if the policy decides to stop.
  */
def retry[T](operation: => T, onRetry: OnRetry[Throwable, T] = emptyOnRetry)(policy: RetryPolicy[Throwable, T]): T =
  retryEither(Try(operation).toEither, onRetry)(policy).fold(throw _, identity)

/** Retries an operation returning an [[scala.util.Either]] until it succeeds or the policy decides to stop. Note that any exceptions thrown
  * by the operation aren't caught and don't cause a retry to happen.
  *
  * @param operation
  *   The operation to retry.
  * @param onRetry
  *   The function to call on each retry - see [[OnRetry]].
  * @param policy
  *   The retry policy - see [[RetryPolicy]].
  * @return
  *   A [[scala.util.Right]] if the function eventually succeeds, or, otherwise, a [[scala.util.Left]] with the error from the last attempt.
  */
def retryEither[E, T](operation: => Either[E, T], onRetry: OnRetry[E, T] = emptyOnRetry)(policy: RetryPolicy[E, T]): Either[E, T] =
  retryWithErrorMode(EitherMode[E])(operation, onRetry)(policy)

/** This is a type alias for a function that represents an action to be performed on each retry attempt.
  *
  * @tparam E
  *   The type of the error that can be returned by the operation.
  * @tparam T
  *   The type of the successful result that can be returned by the operation.
  * @param Int
  *   The attempt number of the retry operation.
  * @param Either[E,
  *   T] The result of the retry operation, represented as an `Either` type. `Left` represents an error and `Right` represents a successful
  *   result.
  * @return
  *   Unit
  */
private[ox] type OnRetry[E, T] = (Int, Either[E, T]) => Unit
private[ox] def emptyOnRetry[E, T] = (_: Int, _: Either[E, T]) => ()

/** Retries an operation using the given error mode until it succeeds or the policy decides to stop. Note that any exceptions thrown by the
  * operation aren't caught (unless the operation catches them as part of its implementation) and don't cause a retry to happen.
  *
  * @param em
  *   The error mode to use, which specifies when a result value is considered success, and when a failure.
  * @param operation
  *   The operation to retry.
  * @param onRetry
  *   The function to call on each retry - see [[OnRetry]].
  * @param policy
  *   The retry policy - see [[RetryPolicy]].
  * @return
  *   Either:
  *   - the result of the function if it eventually succeeds, in the context of `F`, as dictated by the error mode.
  *   - the error `E` in context `F` as returned by the last attempt if the policy decides to stop.
  */
def retryWithErrorMode[E, F[_], T](em: ErrorMode[E, F])(
    operation: => F[T],
    onRetry: OnRetry[E, T] = emptyOnRetry
)(policy: RetryPolicy[E, T]): F[T] =
  @tailrec
  def loop(attempt: Int, remainingAttempts: Option[Int], lastDelay: Option[FiniteDuration]): F[T] =
    def sleepIfNeeded =
      val delay = policy.schedule.nextDelay(attempt, lastDelay).toMillis
      if (delay > 0) Thread.sleep(delay)
      delay

    operation match
      case v if em.isError(v) =>
        val error = em.getError(v)
        onRetry(attempt, Left(error))

        if policy.resultPolicy.isWorthRetrying(error) && remainingAttempts.forall(_ > 0) then
          val delay = sleepIfNeeded
          loop(attempt + 1, remainingAttempts.map(_ - 1), Some(delay.millis))
        else v
      case v =>
        val result = em.getT(v)
        onRetry(attempt, Right(result))

        if !policy.resultPolicy.isSuccess(result) && remainingAttempts.forall(_ > 0) then
          val delay = sleepIfNeeded
          loop(attempt + 1, remainingAttempts.map(_ - 1), Some(delay.millis))
        else v

  val remainingAttempts = policy.schedule match
    case finiteSchedule: Schedule.Finite => Some(finiteSchedule.maxRetries)
    case _                               => None

  loop(1, remainingAttempts, None)
