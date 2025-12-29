package comptime

import zio.test.*

object EitherExtraSpec extends ZIOSpecDefault:
  val spec =
    suite("EitherExtraSpec (comptime)")(
      suite("Either basics")(
        test("map on Right") {
          assertTrue(
            comptime(Right(1).map(_ + 1)) == Right(2),
            comptime((Right(1): Either[String, Int]).map(_ * 2)) == Right(2)
          )
        },
        test("map on Left") {
          assertTrue(
            comptime((Left("error"): Either[String, Int]).map(_ + 1)) == Left("error")
          )
        },
        test("flatMap on Right") {
          assertTrue(
            comptime(Right(1).flatMap(x => Right(x + 1))) == Right(2)
          )
        },
        test("flatMap on Left") {
          assertTrue(
            comptime((Left("error"): Either[String, Int]).flatMap(x => Right(x + 1))) == Left("error")
          )
        },
        test("left.map") {
          assertTrue(
            comptime(Left("error").left.map(_.toUpperCase)) == Left("ERROR"),
            comptime((Right(1): Either[String, Int]).left.map(_.toUpperCase)) == Right(1)
          )
        }
      ),
      suite("filterOrElse")(
        test("Right passing predicate") {
          assertTrue(
            comptime(Right(10).filterOrElse(_ > 5, "too small")) == Right(10)
          )
        },
        test("Right failing predicate") {
          assertTrue(
            comptime(Right(3).filterOrElse(_ > 5, "too small")) == Left("too small")
          )
        },
        test("Left unchanged") {
          assertTrue(
            comptime((Left("error"): Either[String, Int]).filterOrElse(_ > 5, "too small")) == Left("error")
          )
        }
      ),
      suite("getOrElse and orElse")(
        test("getOrElse on Right") {
          assertTrue(
            comptime(Right(1).getOrElse(0)) == 1
          )
        },
        test("getOrElse on Left") {
          assertTrue(
            comptime((Left("error"): Either[String, Int]).getOrElse(0)) == 0
          )
        },
        test("orElse on Right") {
          assertTrue(
            comptime(Right(1).orElse(Right(2))) == Right(1)
          )
        },
        test("orElse on Left") {
          assertTrue(
            comptime((Left("error"): Either[String, Int]).orElse(Right(2))) == Right(2)
          )
        }
      ),
      suite("contains, forall, exists")(
        test("contains") {
          assertTrue(
            comptime(Right(1).contains(1)) == true,
            comptime(Right(1).contains(2)) == false,
            comptime((Left("error"): Either[String, Int]).contains(1)) == false
          )
        },
        test("forall") {
          assertTrue(
            comptime(Right(10).forall(_ > 5)) == true,
            comptime(Right(3).forall(_ > 5)) == false,
            comptime((Left("error"): Either[String, Int]).forall(_ > 5)) == true
          )
        },
        test("exists") {
          assertTrue(
            comptime(Right(10).exists(_ > 5)) == true,
            comptime(Right(3).exists(_ > 5)) == false,
            comptime((Left("error"): Either[String, Int]).exists(_ > 5)) == false
          )
        }
      ),
      suite("toOption and toSeq")(
        test("toOption") {
          assertTrue(
            comptime(Right(1).toOption) == Some(1),
            comptime((Left("error"): Either[String, Int]).toOption) == None
          )
        },
        test("toSeq") {
          assertTrue(
            comptime(Right(1).toSeq) == Seq(1),
            comptime((Left("error"): Either[String, Int]).toSeq) == Seq.empty
          )
        }
      ),
      suite("merge")(
        test("merge Right") {
          assertTrue(
            comptime((Right("hello"): Either[String, String]).merge) == "hello"
          )
        },
        test("merge Left") {
          assertTrue(
            comptime((Left("error"): Either[String, String]).merge) == "error"
          )
        }
      ),
      suite("join operations")(
        test("joinRight") {
          assertTrue(
            comptime((Right(Right(1)): Either[String, Either[String, Int]]).joinRight) == Right(1),
            comptime((Right(Left("inner")): Either[String, Either[String, Int]]).joinRight) == Left("inner"),
            comptime((Left("outer"): Either[String, Either[String, Int]]).joinRight) == Left("outer")
          )
        },
        test("joinLeft") {
          assertTrue(
            comptime((Left(Left("inner")): Either[Either[String, Int], Int]).joinLeft) == Left("inner"),
            comptime((Left(Right(1)): Either[Either[String, Int], Int]).joinLeft) == Right(1),
            comptime((Right(42): Either[Either[String, Int], Int]).joinLeft) == Right(42)
          )
        }
      ),
      suite("swap")(
        test("swap Right") {
          assertTrue(
            comptime(Right(1).swap) == Left(1)
          )
        },
        test("swap Left") {
          assertTrue(
            comptime(Left("error").swap) == Right("error")
          )
        }
      )
    )
