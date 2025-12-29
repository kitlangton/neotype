package neotype

import zio.test.*
import zio.test.Gen

/** Property-based tests using ZIO Test Gen */
object PropertyBasedSpec extends ZIOSpecDefault:

  // Test fixtures
  type PositiveInt = PositiveInt.Type
  object PositiveInt extends Newtype[Int]:
    override inline def validate(n: Int): Boolean = n > 0

  type NonEmptyString = NonEmptyString.Type
  object NonEmptyString extends Newtype[String]:
    override inline def validate(s: String): Boolean = s.nonEmpty

  type BoundedString = BoundedString.Type
  object BoundedString extends Newtype[String]:
    override inline def validate(s: String): Boolean = s.length <= 100

  type EvenInt = EvenInt.Type
  object EvenInt extends Newtype[Int]:
    override inline def validate(n: Int): Boolean = n % 2 == 0

  val spec = suite("PropertyBasedSpec")(
    suite("Newtype validation properties")(
      test("make succeeds for all valid inputs") {
        check(Gen.int(1, Int.MaxValue)) { n =>
          val result = PositiveInt.make(n)
          assertTrue(result.isRight) &&
          assertTrue(result.map(_.unwrap) == Right(n))
        }
      },
      test("make fails for all invalid inputs") {
        check(Gen.int(Int.MinValue, 0)) { n =>
          assertTrue(PositiveInt.make(n).isLeft)
        }
      },
      test("unwrap returns original value") {
        check(Gen.int(1, Int.MaxValue)) { n =>
          val wrapped = PositiveInt.make(n).toOption.get
          assertTrue(wrapped.unwrap == n)
        }
      },
      test("non-empty string accepts any non-empty string") {
        check(Gen.alphaNumericStringBounded(1, 100)) { s =>
          val result = NonEmptyString.make(s)
          assertTrue(result.isRight) &&
          assertTrue(result.map(_.unwrap) == Right(s))
        }
      },
      test("bounded string accepts strings within bound") {
        check(Gen.stringBounded(0, 100)(Gen.char)) { s =>
          val result = BoundedString.make(s)
          assertTrue(result.isRight)
        }
      },
      test("bounded string rejects strings exceeding bound") {
        check(Gen.stringBounded(101, 200)(Gen.alphaChar)) { s =>
          assertTrue(BoundedString.make(s).isLeft)
        }
      },
      test("even int validation is consistent") {
        check(Gen.int) { n =>
          val result = EvenInt.make(n)
          val isEven = n % 2 == 0
          assertTrue(result.isRight == isEven)
        }
      }
    ),
    suite("Roundtrip properties")(
      test("make + unwrap is identity for valid values") {
        check(Gen.int(1, Int.MaxValue)) { n =>
          assertTrue(PositiveInt.make(n).map(_.unwrap) == Right(n))
        }
      },
      test("unsafeMake + unwrap is identity for valid values") {
        check(Gen.int(1, 1000)) { n =>
          val wrapped = PositiveInt.unsafeMake(n) // runtime, no validation
          assertTrue(wrapped.unwrap == n)
        }
      }
    ),
    suite("Boundary value properties")(
      test("edge values around zero") {
        check(Gen.elements(-1, 0, 1)) { n =>
          val result = PositiveInt.make(n)
          assertTrue(result.isRight == (n > 0))
        }
      },
      test("extreme Int values") {
        check(Gen.elements(Int.MinValue, Int.MaxValue, 0, 1, -1)) { n =>
          val result = PositiveInt.make(n)
          assertTrue(result.isRight == (n > 0))
        }
      }
    ),
    suite("Subtype validation properties")(
      test("subtype make succeeds for all valid inputs") {
        check(Gen.int(1, Int.MaxValue)) { n =>
          val result = PositiveIntSubtype.make(n)
          assertTrue(result.isRight) &&
          assertTrue(result.map(identity[Int]) == Right(n))
        }
      },
      test("subtype make fails for all invalid inputs") {
        check(Gen.int(Int.MinValue, 0)) { n =>
          assertTrue(PositiveIntSubtype.make(n).isLeft)
        }
      },
      test("subtype is assignable to base type") {
        check(Gen.int(1, 1000)) { n =>
          val subtype: PositiveIntSubtype = PositiveIntSubtype.make(n).toOption.get
          val asInt: Int                  = subtype
          assertTrue(asInt == n)
        }
      }
    )
  )
