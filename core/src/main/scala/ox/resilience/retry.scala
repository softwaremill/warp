package ox.resilience

import ox.{EitherMode, ErrorMode}
import ox.scheduling.*

import scala.util.Try

/** Retries an operation returning a direct result until it succeeds or the config decides to stop.
  *
  * @param config
  *   The retry config - see [[RetryConfig]].
  * @param operation
  *   The operation to retry.
  * @return
  *   The result of the function if it eventually succeeds.
  * @throws anything
  *   The exception thrown by the last attempt if the config decides to stop.
  */
def retry[T](config: RetryConfig[Throwable, T])(operation: => T): T =
  retryEither(config)(Try(operation).toEither).fold(throw _, identity)

/** Retries an operation returning an [[scala.util.Either]] until it succeeds or the config decides to stop. Note that any exceptions thrown
  * by the operation aren't caught and don't cause a retry to happen.
  *
  * @param config
  *   The retry config - see [[RetryConfig]].
  * @param operation
  *   The operation to retry.
  * @return
  *   A [[scala.util.Right]] if the function eventually succeeds, or, otherwise, a [[scala.util.Left]] with the error from the last attempt.
  */
def retryEither[E, T](config: RetryConfig[E, T])(operation: => Either[E, T]): Either[E, T] =
  retryWithErrorMode(EitherMode[E])(config)(operation)

/** Retries an operation using the given error mode until it succeeds or the config decides to stop. Note that any exceptions thrown by the
  * operation aren't caught (unless the operation catches them as part of its implementation) and don't cause a retry to happen.
  *
  * @param em
  *   The error mode to use, which specifies when a result value is considered success, and when a failure.
  * @param config
  *   The retry config - see [[RetryConfig]].
  * @param operation
  *   The operation to retry.
  * @return
  *   Either:
  *   - the result of the function if it eventually succeeds, in the context of `F`, as dictated by the error mode.
  *   - the error `E` in context `F` as returned by the last attempt if the config decides to stop.
  */
def retryWithErrorMode[E, F[_], T](em: ErrorMode[E, F])(config: RetryConfig[E, T])(operation: => F[T]): F[T] =
  scheduledWithErrorMode(em)(config.toScheduledConfig)(operation)
