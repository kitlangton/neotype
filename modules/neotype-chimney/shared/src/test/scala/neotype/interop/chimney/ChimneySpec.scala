package neotype.interop.chimney

import io.scalaland.chimney.dsl.*
import neotype.{Newtype, PositiveIntNewtype, Subtype}
import neotype.test.definitions.*
import zio.test.*

object ChimneySpec extends ZIOSpecDefault:
  def spec = suite("ChimneySpec")(
    suite("NonEmptyString")(
      test("partially transform success") {
        val string = "hello"
        val result = string.transformIntoPartial[ValidatedNewtype]
        assertTrue(result.asEither == Right(ValidatedNewtype("hello")))
      },
      test("partially transform failure") {
        val string = ""
        val result = string.transformIntoPartial[ValidatedNewtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("String must not be empty"))
        )
      },
      test("transform the other way") {
        val newtype = ValidatedNewtype("hello")
        val result  = newtype.transformInto[String]
        assertTrue(result == "hello")
      }
    ),
    suite("SubtypeLongString")(
      test("partially transform success") {
        val string = "hello world"
        val result = string.transformIntoPartial[ValidatedSubtype]
        assertTrue(result.asEither == Right(ValidatedNewtype("hello world")))
      },
      test("partially transform failure") {
        val string = "hello"
        val result = string.transformIntoPartial[ValidatedSubtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("String must be longer than 10 characters"))
        )
      }
    ),
    suite("SimpleNewtype")(
      test("transform to newtype successfully") {
        val int    = 123
        val result = int.transformInto[SimpleNewtype]
        assertTrue(result == SimpleNewtype(123))
      },
      test("transform from newtype to inner type") {
        val newtype = SimpleNewtype(123)
        val result  = newtype.transformInto[Int]
        assertTrue(result == 123)
      },
      test("transform from simple newtype to same type") {
        val newtype = SimpleNewtype(123)
        val result  = newtype.transformInto[SimpleNewtype]
        assertTrue(result == newtype)
      },
      test("transform from simple newtype to another simple newtype") {
        type SimpleNewtype2 = SimpleNewtype2.Type
        object SimpleNewtype2 extends Newtype[Int]

        val newtype = SimpleNewtype(123)
        val result  = newtype.transformInto[SimpleNewtype2]
        assertTrue(result == SimpleNewtype2(123))
      },
      test("partially transform from simple newtype to validated newtype (success)") {
        val newtype = SimpleNewtype(123)
        val result  = newtype.transformIntoPartial[PositiveIntNewtype]
        assertTrue(result.asEither == Right(PositiveIntNewtype(123)))
      },
      test("partially transform from simple newtype to validated newtype (failure)") {
        val newtype = SimpleNewtype(0)
        val result  = newtype.transformIntoPartial[PositiveIntNewtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("Validation Failed"))
        )
      }
    ),
    suite("SimpleSubtype")(
      test("transform to subtype successfully") {
        val int    = 123
        val result = int.transformInto[SimpleSubtype]
        assertTrue(result == SimpleSubtype(123))
      },
      test("transform from subtype to inner type") {
        val subtype = SimpleSubtype(123)
        val result  = subtype.transformInto[Int]
        assertTrue(result == 123)
      },
      test("transform from simple subtype to same type") {
        val subtype = SimpleSubtype(123)
        val result  = subtype.transformInto[SimpleSubtype]
        assertTrue(result == subtype)
      },
      test("transform from simple subtype to another simple subtype") {
        type SimpleSubtype2 = SimpleSubtype2.Type
        object SimpleSubtype2 extends Subtype[Int]

        val subtype = SimpleSubtype(123)
        val result  = subtype.transformInto[SimpleSubtype2]
        assertTrue(result == SimpleSubtype2(123))
      },
      test("partially transform from simple subtype to validated subtype (success)") {
        val subtype = SimpleSubtype(123)
        val result  = subtype.transformIntoPartial[PositiveIntSubtype]
        assertTrue(result.asEither == Right(PositiveIntSubtype(123)))
      },
      test("partially transform from simple subtype to validated subtype (failure)") {
        val subtype = SimpleSubtype(0)
        val result  = subtype.transformIntoPartial[PositiveIntSubtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("Validation Failed"))
        )
      }
    ),
    suite("PositiveIntNewtype")(
      test("transform from validated to inner") {
        val validated = PositiveIntNewtype(123)
        val inner     = validated.transformInto[Int]
        assertTrue(inner == 123)
      },
      test("transform from validated to simple") {
        val validated = PositiveIntNewtype(123)
        val simple    = validated.transformInto[SimpleNewtype]
        assertTrue(simple == SimpleNewtype(123))
      },
      test("partially transform from validated to another validated newtype (success)") {
        val validated = PositiveIntNewtype(123)
        val result    = validated.transformIntoPartial[GreaterThan10Newtype]
        assertTrue(result.asEither == Right(GreaterThan10Newtype(123)))
      },
      test("partially transform from validated to another validated newtype (failure)") {
        val validated = PositiveIntNewtype(5)
        val result    = validated.transformIntoPartial[GreaterThan10Newtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("Validation Failed"))
        )
      }
    ),
    suite("PositiveIntSubtype")(
      test("transform from validated to inner") {
        val validated = PositiveIntSubtype(123)
        val inner     = validated.transformInto[Int]
        assertTrue(inner == 123)
      },
      test("transform from validated to simple") {
        val validated = PositiveIntSubtype(123)
        val simple    = validated.transformInto[SimpleSubtype]
        assertTrue(simple == SimpleSubtype(123))
      },
      test("partially transform from validated to another validated subtype (success)") {
        val validated = PositiveIntSubtype(123)
        val result    = validated.transformIntoPartial[GreaterThan10Subtype]
        assertTrue(result.asEither == Right(GreaterThan10Subtype(123)))
      },
      test("partially transform from validated to another validated subtype (failure)") {
        val validated = PositiveIntSubtype(5)
        val result    = validated.transformIntoPartial[GreaterThan10Subtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("Validation Failed"))
        )
      }
    ),
    suite("Cross-type transformations")(
      test("transform from newtype to simple subtype") {
        val newtype = SimpleNewtype(123)
        val result  = newtype.transformInto[SimpleSubtype]
        assertTrue(result == SimpleSubtype(123))
      },
      test("partially transform from newtype to validated subtype (success)") {
        val newtype = SimpleNewtype(123)
        val result  = newtype.transformIntoPartial[PositiveIntSubtype]
        assertTrue(result.asEither == Right(PositiveIntSubtype(123)))
      },
      test("partially transform from newtype to validated subtype (failure)") {
        val newtype = SimpleNewtype(0)
        val result  = newtype.transformIntoPartial[PositiveIntSubtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("Validation Failed"))
        )
      },
      test("transform from subtype to simple newtype") {
        val subtype = SimpleSubtype(123)
        val result  = subtype.transformInto[SimpleNewtype]
        assertTrue(result == SimpleNewtype(123))
      },
      test("partially transform from subtype to validated newtype (success)") {
        val subtype = SimpleSubtype(123)
        val result  = subtype.transformIntoPartial[PositiveIntNewtype]
        assertTrue(result.asEither == Right(PositiveIntNewtype(123)))
      },
      test("partially transform from subtype to validated newtype (failure)") {
        val subtype = SimpleSubtype(0)
        val result  = subtype.transformIntoPartial[PositiveIntNewtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("Validation Failed"))
        )
      },
      test("partially transform from validated newtype to validated subtype (success)") {
        val validatedNewtype = PositiveIntNewtype(123)
        val result           = validatedNewtype.transformIntoPartial[PositiveIntSubtype]
        assertTrue(result.asEither == Right(PositiveIntSubtype(123)))
      },
      test("partially transform from validated newtype to validated subtype (failure)") {
        val validatedNewtype = PositiveIntNewtype(5)
        val result           = validatedNewtype.transformIntoPartial[GreaterThan10Subtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("Validation Failed"))
        )
      },
      test("partially transform from validated subtype to validated newtype (success)") {
        val validatedSubtype = PositiveIntSubtype(123)
        val result           = validatedSubtype.transformIntoPartial[PositiveIntNewtype]
        assertTrue(result.asEither == Right(PositiveIntNewtype(123)))
      },
      test("partially transform from validated subtype to validated newtype (failure)") {
        val validatedSubtype = PositiveIntSubtype(5)
        val result           = validatedSubtype.transformIntoPartial[GreaterThan10Newtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("Validation Failed"))
        )
      }
    ),
    suite("Compile-time validation")(
      test("not compile invalid transformations for newtype") {
        for errorNewtype <- typeCheck("\"hello\".transformInto[ValidatedNewtype]").absolve.flip
        yield assertTrue(errorNewtype.nonEmpty)
      },
      test("not compile invalid transformations for subtype") {
        for errorSubtype <- typeCheck("\"hello\".transformInto[ValidatedSubtype]").absolve.flip
        yield assertTrue(errorSubtype.nonEmpty)
      }
    )
  )

  type PositiveIntSubtype = PositiveIntSubtype.Type
  object PositiveIntSubtype extends Subtype[Int]:
    override inline def validate(int: Int): Boolean = int > 0

  type GreaterThan10Newtype = GreaterThan10Newtype.Type
  object GreaterThan10Newtype extends Newtype[Int]:
    override inline def validate(int: Int): Boolean = int > 10

  type GreaterThan10Subtype = GreaterThan10Subtype.Type
  object GreaterThan10Subtype extends Subtype[Int]:
    override inline def validate(int: Int): Boolean = int > 10
