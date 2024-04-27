package ox.resilience

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ox.ElapsedTime

import scala.concurrent.duration.*

class ScheduleCombinationRetryTest extends AnyFlatSpec with Matchers with ElapsedTime:
  behavior of "retry with combination of schedules"

  it should "retry 3 times immediately and then 2 times with delay" in {
    // given
    var counter = 0
    val sleep = 100.millis
    val immediateRetries = 3
    val delayedRetries = 2

    def f =
      counter += 1
      throw new RuntimeException("boom")

    val schedule = Schedule.Immediate(immediateRetries).fallbackTo(Schedule.Delay(delayedRetries, sleep))

    // when
    val (result, elapsedTime) = measure(the[RuntimeException] thrownBy retry(RetryPolicy(schedule))(f))

    // then
    result should have message "boom"
    counter shouldBe immediateRetries + delayedRetries + 1
    elapsedTime.toMillis should be >= 2 * sleep.toMillis
  }

  it should "retry forever" in {
    // given
    var counter = 0
    val retriesUntilSuccess = 1_000
    val successfulResult = 42

    def f =
      counter += 1
      if counter <= retriesUntilSuccess then throw new RuntimeException("boom") else successfulResult

    val schedule = Schedule.Immediate(100).fallbackTo(Schedule.Delay.forever(2.millis))

    // when
    val result = retry(RetryPolicy(schedule))(f)

    // then
    result shouldBe successfulResult
    counter shouldBe retriesUntilSuccess + 1
  }
