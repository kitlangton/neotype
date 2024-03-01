package neotype.circe

import neotype.{Newtype, Subtype}
import zio.test.*
import io.circe.*
import io.circe.parser.*

type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  override inline def validate(value: String): Boolean =
    value.nonEmpty

  override inline def failureMessage = "String must not be empty"

type SubtypeLongString = SubtypeLongString.Type
given SubtypeLongString: Subtype[String] with
  override inline def validate(value: String): Boolean =
    value.length > 10

  override inline def failureMessage = "String must be longer than 10 characters"

type SimpleNewtype = SimpleNewtype.Type
given SimpleNewtype: Newtype.Simple[Int]()

type SimpleSubtype = SimpleSubtype.Type
given SimpleSubtype: Subtype.Simple[String]()

object CirceJsonSpec extends ZIOSpecDefault:
  def spec = suite("CirceJsonSpec")(
    suite("NonEmptyString")(
      test("parse success") {
        val json   = """ "hello" """
        val parsed = decode[NonEmptyString](json)
        assertTrue(parsed == Right(NonEmptyString("hello")))
      },
      test("parse failure") {
        val json   = """ "" """
        val parsed = decode[NonEmptyString](json)
        assertTrue(parsed.left.map(_.getMessage) == Left("DecodingFailure at : String must not be empty"))
      }
    ),
    suite("SubtypeLongString")(
      test("parse success") {
        val json   = """ "hello world" """
        val parsed = decode[SubtypeLongString](json)
        assertTrue(parsed == Right(SubtypeLongString("hello world")))
      },
      test("parse failure") {
        val json   = """ "hello" """
        val parsed = decode[SubtypeLongString](json)
        assertTrue(
          parsed.left.map(_.getMessage) == Left("DecodingFailure at : String must be longer than 10 characters")
        )
      }
    ),
    suite("SimpleNewtype")(
      test("parse success") {
        val json   = """123"""
        val parsed = decode[SimpleNewtype](json)
        assertTrue(parsed == Right(SimpleNewtype(123)))
      },
      test("parse failure") {
        val json   = """ "hello" """
        val parsed = decode[SimpleNewtype](json)
        assertTrue(parsed.isLeft)
      }
    ),
    suite("SimpleSubtype")(
      test("parse success") {
        val json   = """ "hello" """
        val parsed = decode[SimpleSubtype](json)
        assertTrue(parsed == Right(SimpleSubtype("hello")))
      },
      test("parse failure") {
        val json   = """123"""
        val parsed = decode[SimpleSubtype](json)
        assertTrue(parsed.isLeft)
      }
    )
  )
