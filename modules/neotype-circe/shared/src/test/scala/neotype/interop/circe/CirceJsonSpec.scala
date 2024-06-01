package neotype.interop.circe

import io.circe.*
import io.circe.parser.*
import neotype.Newtype
import neotype.Subtype
import neotype.interop.circe.given_Decoder_B
import neotype.test.definitions.*
import zio.test.*

object CirceJsonSpec extends ZIOSpecDefault:
  def spec = suite("CirceJsonSpec")(
    suite("NonEmptyString")(
      test("parse success") {
        val json   = """ "hello" """
        val parsed = decode[ValidatedNewtype](json)
        assertTrue(parsed == Right(ValidatedNewtype("hello")))
      },
      test("parse failure") {
        val json   = """ "" """
        val parsed = decode[ValidatedNewtype](json)
        assertTrue(parsed.left.map(_.getMessage) == Left("DecodingFailure at : String must not be empty"))
      }
    ),
    suite("SubtypeLongString")(
      test("parse success") {
        val json   = """ "hello world" """
        val parsed = decode[ValidatedSubtype](json)
        assertTrue(parsed == Right(ValidatedSubtype("hello world")))
      },
      test("parse failure") {
        val json   = """ "hello" """
        val parsed = decode[ValidatedSubtype](json)
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
        val json   = """ 123 """
        val parsed = decode[SimpleSubtype](json)
        assertTrue(parsed == Right(SimpleSubtype(123)))
      },
      test("parse failure") {
        val json   = """ "hello" """
        val parsed = decode[SimpleSubtype](json)
        assertTrue(parsed.isLeft)
      }
    )
  )
