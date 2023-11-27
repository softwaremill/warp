package ox.retry

import org.scalatest.{EitherValues, TryValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ox.retry.*

import scala.util.{Failure, Success}

class DirectRetryTest extends AnyFlatSpec with EitherValues with TryValues with Matchers:

  behavior of "Direct retry"

  it should "retry a succeeding function" in {
    // given
    var counter = 0
    val successfulResult = 42

    def f =
      counter += 1
      successfulResult

    // when
    val result = retry(f)(RetryPolicy.Direct(3))

    // then
    result shouldBe successfulResult
    counter shouldBe 1
  }

  it should "fail fast when a function is not worth retrying" in {
    // given
    var counter = 0
    val errorMessage = "boom"

    def f =
      counter += 1
      if true then throw new RuntimeException(errorMessage)

    // when/then
    the[RuntimeException] thrownBy retry(f, isWorthRetrying = _ => false)(RetryPolicy.Direct(3)) should have message errorMessage
    counter shouldBe 1
  }

  it should "retry a succeeding function with a custom success condition" in {
    // given
    var counter = 0
    val unsuccessfulResult = -1

    def f =
      counter += 1
      unsuccessfulResult

    // when
    val result = retry(f, isSuccess = _ > 0)(RetryPolicy.Direct(3))

    // then
    result shouldBe unsuccessfulResult
    counter shouldBe 4
  }

  it should "retry a failing function" in {
    // given
    var counter = 0
    val errorMessage = "boom"

    def f =
      counter += 1
      if true then throw new RuntimeException(errorMessage)

    // when/then
    the[RuntimeException] thrownBy retry(f)(RetryPolicy.Direct(3)) should have message errorMessage
    counter shouldBe 4
  }

  it should "retry a failing function forever" in {
    // given
    var counter = 0
    val retriesUntilSuccess = 10_000
    val successfulResult = 42

    def f =
      counter += 1
      if counter <= retriesUntilSuccess then throw new RuntimeException("boom") else successfulResult

    // when
    val result = retry(f)(RetryPolicy.Direct.forever)

    // then
    result shouldBe successfulResult
    counter shouldBe retriesUntilSuccess + 1
  }

  it should "retry a succeeding Either" in {
    // given
    var counter = 0
    val successfulResult = 42

    def f: Either[String, Int] =
      counter += 1
      Right(successfulResult)

    // when
    val result = retry(f)(RetryPolicy.Direct(3))

    // then
    result.value shouldBe successfulResult
    counter shouldBe 1
  }

  it should "fail fast when an Either is not worth retrying" in {
    // given
    var counter = 0
    val errorMessage = "boom"

    def f: Either[String, Int] =
      counter += 1
      Left(errorMessage)

    // when
    val result = retry(f, isWorthRetrying = (_: String) => false)(RetryPolicy.Direct(3))

    // then
    result.left.value shouldBe errorMessage
    counter shouldBe 1
  }

  it should "retry a succeeding Either with a custom success condition" in {
    // given
    var counter = 0
    val unsuccessfulResult = -1

    def f: Either[String, Int] =
      counter += 1
      Right(unsuccessfulResult)

    // when
    val result = retry(f, isSuccess = (res: Int) => res > 0)(RetryPolicy.Direct(3))

    // then
    result.value shouldBe unsuccessfulResult
    counter shouldBe 4
  }

  it should "retry a failing Either" in {
    // given
    var counter = 0
    val errorMessage = "boom"

    def f =
      counter += 1
      Left(errorMessage)

    // when
    val result = retry(f)(RetryPolicy.Direct(3))

    // then
    result.left.value shouldBe errorMessage
    counter shouldBe 4
  }

  it should "retry a succeeding Try" in {
    // given
    var counter = 0
    val successfulResult = 42

    def f =
      counter += 1
      Success(successfulResult)

    // when
    val result = retry(f)(RetryPolicy.Direct(3))

    // then
    result.success.value shouldBe successfulResult
    counter shouldBe 1
  }

  it should "fail fast when a Try is not worth retrying" in {
    // given
    var counter = 0
    val errorMessage = "boom"

    def f =
      counter += 1
      Failure(new RuntimeException(errorMessage))

    // when
    val result = retry(f, isWorthRetrying = (_: Throwable) => false)(RetryPolicy.Direct(3))

    // then
    result.failure.exception should have message errorMessage
    counter shouldBe 1
  }

  it should "retry a succeeding Try with a custom success condition" in {
    // given
    var counter = 0
    val unsuccessfulResult = -1

    def f =
      counter += 1
      Success(unsuccessfulResult)

    // when
    val result = retry(f, isSuccess = (res: Int) => res > 0)(RetryPolicy.Direct(3))

    // then
    result.success.value shouldBe unsuccessfulResult
    counter shouldBe 4
  }

  it should "retry a failing Try" in {
    // given
    var counter = 0
    val errorMessage = "boom"

    def f =
      counter += 1
      Failure(new RuntimeException(errorMessage))

    // when
    val result = retry(f)(RetryPolicy.Direct(3))

    // then
    result.failure.exception should have message errorMessage
    counter shouldBe 4
  }
