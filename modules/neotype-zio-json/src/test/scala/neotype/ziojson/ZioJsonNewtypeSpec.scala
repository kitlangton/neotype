package neotype.ziojson

import neotype.{Newtype, Subtype}
import zio.test.*
import zio.json.*

type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  inline def validate(value: String): Boolean =
    value.nonEmpty

  override inline def failureMessage = "String must not be empty"

type SubtypeLongString = SubtypeLongString.Type
given SubtypeLongString: Subtype[String] with
  inline def validate(value: String): Boolean =
    value.length > 10

  override inline def failureMessage = "String must be longer than 10 characters"

type SimpleNewtype = SimpleNewtype.Type
given SimpleNewtype: Newtype.Simple[Int] with {}

type SimpleSubtype = SimpleSubtype.Type
given SimpleSubtype: Subtype.Simple[String] with {}

object ZioJsonSpec extends ZIOSpecDefault:
  def spec = suite("ZioJsonSpec")(
    suite("NonEmptyString")(
      test("parse success") {
        val json   = """ "hello" """
        val parsed = json.fromJson[NonEmptyString]
        assertTrue(parsed == Right(NonEmptyString("hello")))
      },
      test("parse failure") {
        val json   = """ "" """
        val parsed = json.fromJson[NonEmptyString]
        assertTrue(parsed == Left("(String must not be empty)"))
      }
    ),
    suite("SubtypeLongString")(
      test("parse success") {
        val json   = """ "hello world" """
        val parsed = json.fromJson[SubtypeLongString]
        assertTrue(parsed == Right(SubtypeLongString("hello world")))
      },
      test("parse failure") {
        val json   = """ "hello" """
        val parsed = json.fromJson[SubtypeLongString]
        assertTrue(parsed == Left("(String must be longer than 10 characters)"))
      }
    ),
    suite("SimpleNewtype")(
      test("parse success") {
        val json   = """ 123 """
        val parsed = json.fromJson[SimpleNewtype]
        assertTrue(parsed == Right(SimpleNewtype(123)))
      },
      test("parse failure") {
        val json   = """ "hello" """
        val parsed = json.fromJson[SimpleNewtype]
        assertTrue(parsed.isLeft)
      }
    ),
    suite("SimpleSubtype")(
      test("parse success") {
        val json   = """ "hello" """
        val parsed = json.fromJson[SimpleSubtype]
        assertTrue(parsed == Right(SimpleSubtype("hello")))
      },
      test("parse failure") {
        val json   = """ 123 """
        val parsed = json.fromJson[SimpleSubtype]
        assertTrue(parsed.isLeft)
      }
    )
  )
