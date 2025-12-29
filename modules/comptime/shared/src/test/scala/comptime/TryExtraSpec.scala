package comptime

import zio.test.*

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object TryExtraSpec extends ZIOSpecDefault:
  val spec =
    suite("TryExtraSpec (comptime)")(
      suite("toOption")(
        test("Success converts to Some") {
          assertTrue(
            comptime(Success(1).toOption) == Some(1),
            comptime(Success("hello").toOption) == Some("hello"),
            comptime(Try(42).toOption) == Some(42)
          )
        },
        test("Failure converts to None") {
          assertTrue(
            comptime(Try(1 / 0).toOption) == None
            // RuntimeException.<init> not supported
          )
        }
      ),
      // TODO: toEither returns Either[Throwable, ...] which can't be lifted (no ToExpr for Throwable)
      // suite("toEither")(
      //   test("Success converts to Right") {
      //     assertTrue(
      //       comptime(Success(1).toEither) == Right(1),
      //       comptime(Success("hello").toEither) == Right("hello"),
      //       comptime(Try(42).toEither) == Right(42)
      //     )
      //   },
      //   test("Failure converts to Left with exception") {
      //     val result = comptime(Try(1 / 0).toEither)
      //     assertTrue(
      //       result.isLeft,
      //       result.left.toOption.exists(_.isInstanceOf[ArithmeticException])
      //     )
      //   }
      // ),
      suite("fold")(
        test("fold on Success applies success function") {
          assertTrue(
            comptime(Success(10).fold(_ => -1, x => x + 1)) == 11,
            comptime(Try(5).fold(_ => 0, _ * 2)) == 10
          )
        },
        test("fold on Failure applies failure function") {
          assertTrue(
            comptime(Try(1 / 0).fold(_ => -1, x => x)) == -1
            // RuntimeException.<init> not supported
          )
        }
      ),
      // TODO: failed returns Try[Throwable] which can't be lifted
      // suite("failed")(
      //   test("failed on Failure returns Success with exception") {
      //     val result = comptime(Try(1 / 0).failed)
      //     assertTrue(
      //       result.isSuccess,
      //       result.toOption.exists(_.isInstanceOf[ArithmeticException])
      //     )
      //   },
      //   test("failed on Success returns Failure with UnsupportedOperationException") {
      //     val result = comptime(Success(1).failed)
      //     assertTrue(
      //       result.isFailure,
      //       result.failed.toOption.exists(_.isInstanceOf[UnsupportedOperationException])
      //     )
      //   }
      // ),
      suite("transform")(
        test("transform on Success applies success function") {
          assertTrue(
            comptime(Success(10).transform(x => Success(x + 1), _ => Success(-1))) == Success(11),
            comptime(Try(5).transform(x => Success(x * 2), _ => Success(0))) == Success(10)
          )
        },
        test("transform on Failure applies failure function") {
          assertTrue(
            comptime(Try(1 / 0).transform(_ => Success(100), _ => Success(-1))) == Success(-1)
          )
        },
        test("transform can turn Success into Failure") {
          val result = comptime(Success(10).transform(_ => Try(1 / 0), e => Success(-1)))
          assertTrue(result.isFailure)
        },
        test("transform can turn Failure into Failure") {
          val result = comptime(Try(1 / 0).transform(_ => Success(1), _ => Try(2 / 0)))
          assertTrue(result.isFailure)
        }
      ),
      suite("recover")(
        test("recover on Failure applies partial function") {
          assertTrue(
            comptime(Try(1 / 0).recover { case _: ArithmeticException => -1 }) == Success(-1)
            // IllegalArgumentException.<init> not supported
          )
        },
        test("recover on Success returns original") {
          assertTrue(
            comptime(Success(10).recover { case _ => -1 }) == Success(10)
          )
        },
        test("recover does not apply if exception type does not match") {
          val result = comptime(Try(1 / 0).recover { case _: IllegalArgumentException => -1 })
          assertTrue(result.isFailure)
        }
      ),
      suite("recoverWith")(
        test("recoverWith on Failure applies partial function") {
          assertTrue(
            comptime(Try(1 / 0).recoverWith { case _: ArithmeticException => Success(-1) }) == Success(-1)
            // IllegalArgumentException.<init> not supported
          )
        },
        test("recoverWith on Success returns original") {
          assertTrue(
            comptime(Success(10).recoverWith { case _ => Success(-1) }) == Success(10)
          )
        },
        test("recoverWith does not apply if exception type does not match") {
          val result = comptime(Try(1 / 0).recoverWith { case _: IllegalArgumentException => Success(-1) })
          assertTrue(result.isFailure)
        },
        test("recoverWith can return Failure") {
          val result = comptime(Try(1 / 0).recoverWith { case _: ArithmeticException => Try(2 / 0) })
          assertTrue(result.isFailure)
        }
      ),
      // Existing functionality tests to ensure we don't break anything
      suite("existing Try operations")(
        test("isSuccess and isFailure") {
          assertTrue(
            comptime(Success(1).isSuccess) == true,
            comptime(Success(1).isFailure) == false,
            comptime(Try(1 / 0).isSuccess) == false,
            comptime(Try(1 / 0).isFailure) == true
          )
        },
        test("getOrElse") {
          assertTrue(
            comptime(Success(10).getOrElse(0)) == 10,
            comptime(Try(1 / 0).getOrElse(-1)) == -1,
            comptime(Success(5).getOrElse(1 / 0)) == 5 // by-name: fallback not evaluated
          )
        },
        test("map") {
          assertTrue(
            comptime(Success(10).map(_ + 1)) == Success(11),
            comptime(Try(1 / 0).map(_ + 1).isFailure) == true
          )
        },
        test("flatMap") {
          assertTrue(
            comptime(Success(10).flatMap(x => Success(x + 1))) == Success(11),
            comptime(Try(1 / 0).flatMap(x => Success(x + 1)).isFailure) == true
          )
        }
      )
    )
