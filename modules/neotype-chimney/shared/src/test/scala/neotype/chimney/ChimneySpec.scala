package neotype.chimney

import io.scalaland.chimney.dsl.*
import neotype.Newtype
import neotype.Subtype
import neotype.test.definitions.*
import zio.test.*

object ChimneySpec extends ZIOSpecDefault:
  def spec = suite("ChimneySpec")(
    suite("NonEmptyString")(
      test("transform success") {
        val string = "hello"
        val result = string.transformIntoPartial[ValidatedNewtype]
        assertTrue(result.asEither == Right(ValidatedNewtype("hello")))
      },
      test("transform failure") {
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
      test("transform success") {
        val string = "hello world"
        val result = string.transformIntoPartial[ValidatedSubtype]
        assertTrue(result.asEither == Right(ValidatedNewtype("hello world")))
      },
      test("transform failure") {
        val string = "hello"
        val result = string.transformIntoPartial[ValidatedSubtype]
        assertTrue(
          result.asEitherErrorPathMessages.left.map(_.map(_._2.asString).toList) ==
            Left(List("String must be longer than 10 characters"))
        )
      }
    ),
    suite("SimpleNewtype")(
      test("transform success") {
        val int    = 123
        val result = int.transformIntoPartial[SimpleNewtype]
        assertTrue(result.asEither == Right(SimpleNewtype(123)))
      },
      test("transform the other way") {
        val newtype = SimpleNewtype(123)
        val result  = newtype.transformInto[Int]
        assertTrue(result == 123)
      }
    ),
    suite("SimpleSubtype")(
      test("transform success") {
        val int    = 123
        val result = int.transformIntoPartial[SimpleSubtype]
        assertTrue(result.asEither == Right(SimpleSubtype(123)))
      }
    )
  )
