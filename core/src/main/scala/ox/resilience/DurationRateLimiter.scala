package ox.resilience

import ox.{Ox, forkDiscard}
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

/** Rate limiter with a customizable algorithm. Operations can be blocked or dropped, when the rate limit is reached. operationMode decides
  * if whole time of execution should be considered or just the start.
  */
class DurationRateLimiter private (algorithm: DurationRateLimiterAlgorithm):
  /** Runs the operation, blocking if the rate limit is reached, until the rate limiter is replenished. */
  def runBlocking[T](operation: => T): T =
    algorithm.acquire()
    algorithm.startOperation()
    val result = operation
    algorithm.endOperation()
    result

  /** Runs or drops the operation, if the rate limit is reached.
    *
    * @return
    *   `Some` if the operation has been allowed to run, `None` if the operation has been dropped.
    */
  def runOrDrop[T](operation: => T): Option[T] =
    if algorithm.tryAcquire() then
      algorithm.startOperation()
      val result = operation
      algorithm.endOperation()
      Some(result)
    else None

end DurationRateLimiter

object DurationRateLimiter:
  def apply(algorithm: DurationRateLimiterAlgorithm)(using Ox): DurationRateLimiter =
    @tailrec
    def update(): Unit =
      val waitTime = algorithm.getNextUpdate
      val millis = waitTime / 1000000
      val nanos = waitTime % 1000000
      Thread.sleep(millis, nanos.toInt)
      algorithm.update()
      update()
    end update

    forkDiscard(update())
    new DurationRateLimiter(algorithm)
  end apply

  /** Creates a rate limiter using a fixed window algorithm.
    *
    * Must be run within an [[Ox]] concurrency scope, as a background fork is created, to replenish the rate limiter.
    *
    * @param maxOperations
    *   Maximum number of operations that are allowed to **start** within a time [[window]].
    * @param window
    *   Interval of time between replenishing the rate limiter. The rate limiter is replenished to allow up to [[maxOperations]] in the next
    *   time window.
    */
  def fixedWindow(maxOperations: Int, window: FiniteDuration)(using
      Ox
  ): DurationRateLimiter =
    apply(DurationRateLimiterAlgorithm.FixedWindow(maxOperations, window))

  /** Creates a rate limiter using a sliding window algorithm.
    *
    * Must be run within an [[Ox]] concurrency scope, as a background fork is created, to replenish the rate limiter.
    *
    * @param maxOperations
    *   Maximum number of operations that are allowed to **start** within any [[window]] of time.
    * @param window
    *   Length of the window.
    */
  def slidingWindow(maxOperations: Int, window: FiniteDuration)(using Ox): DurationRateLimiter =
    apply(DurationRateLimiterAlgorithm.SlidingWindow(maxOperations, window))

  /** Creates a rate limiter with token/leaky bucket algorithm.
    *
    * Must be run within an [[Ox]] concurrency scope, as a background fork is created, to replenish the rate limiter.
    *
    * @param maxTokens
    *   Max capacity of tokens in the algorithm, limiting the operations that are allowed to **start** concurrently.
    * @param refillInterval
    *   Interval of time between adding a single token to the bucket.
    */
  def leakyBucket(maxTokens: Int, refillInterval: FiniteDuration)(using Ox): DurationRateLimiter =
    apply(DurationRateLimiterAlgorithm.LeakyBucket(maxTokens, refillInterval))
end DurationRateLimiter
