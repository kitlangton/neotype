package neotype.ziojson

import neotype.*
import zio.test.*
import zio.json.*
import neotype.test.definitions.*

object ZioJsonSpec extends ZIOSpecDefault:
  given JsonCodec[Composite] = DeriveJsonCodec.gen

  def spec = suite("ZioJsonSpec")(
    suite("NonEmptyString")(
      test("parse success") {
        val json   = """ "hello" """
        val parsed = json.fromJson[ValidatedNewtype]
        assertTrue(parsed == Right(ValidatedNewtype("hello")))
      },
      test("parse failure") {
        val json   = """ "" """
        val parsed = json.fromJson[ValidatedNewtype]
        assertTrue(parsed == Left("(String must not be empty)"))
      },
      test("toJson") {
        val json = ValidatedNewtype("hello").toJson
        assertTrue(json == """"hello"""")
      }
    ),
    suite("SubtypeLongString")(
      test("parse success") {
        val json   = """ "hello world" """
        val parsed = json.fromJson[ValidatedSubtype]
        assertTrue(parsed == Right(ValidatedSubtype("hello world")))
      },
      test("parse failure") {
        val json   = """ "hello" """
        val parsed = json.fromJson[ValidatedSubtype]
        assertTrue(parsed == Left("(String must be longer than 10 characters)"))
      },
      test("toJson") {
        val json = ValidatedSubtype("hello world").toJson
        assertTrue(json == """"hello world"""")
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
      },
      test("toJson") {
        val json = SimpleNewtype(123).toJson
        assertTrue(json == "123")
      }
    ),
    suite("SimpleSubtype")(
      test("parse success") {
        val json   = """ 123 """
        val parsed = json.fromJson[SimpleSubtype]
        assertTrue(parsed == Right(SimpleSubtype(123)))
      },
      test("parse failure") {
        val json   = """ "WHOOPS" """
        val parsed = json.fromJson[SimpleSubtype]
        assertTrue(parsed.isLeft)
      },
      test("toJson") {
        val json = SimpleSubtype(123).toJson
        assertTrue(json == "123")
      }
    ),
    suite("Composite")(
      test("parse success") {
        val json   = """ { "newtype": "hello", "simpleNewtype": 123, "subtype": "hello world", "simpleSubtype": 123 } """
        val parsed = json.fromJson[Composite]
        assertTrue(
          parsed == Right(
            Composite(
              ValidatedNewtype("hello"),
              SimpleNewtype(123),
              ValidatedSubtype("hello world"),
              SimpleSubtype(123)
            )
          )
        )
      },
      test("parse failure") {
        val json   = """ { "newtype": "", "simpleNewtype": 123, "subtype": "hello", "simpleSubtype": "WHOOPS" } """
        val parsed = json.fromJson[Composite]
        assertTrue(parsed == Left(".newtype(String must not be empty)"))
      },
      test("toJson") {
        val json = Composite(
          ValidatedNewtype("hello"),
          SimpleNewtype(123),
          ValidatedSubtype("hello world"),
          SimpleSubtype(123)
        ).toJson
        assertTrue(
          json == """{"newtype":"hello","simpleNewtype":123,"subtype":"hello world","simpleSubtype":123}"""
        )
      }
    )
  )
