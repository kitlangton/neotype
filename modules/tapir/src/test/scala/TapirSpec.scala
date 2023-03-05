package neotype.tapir

import zio.test.*
import neotype.Newtype
import neotype.Subtype
import sttp.tapir.Schema
import sttp.tapir.Codec
import sttp.tapir.DecodeResult
import zio.test.Assertion.*

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
given SimpleNewtype: Newtype.Simple[Int]()

type SimpleSubtype = SimpleSubtype.Type
given SimpleSubtype: Subtype.Simple[Int]()

type Id[A] = A

object TapirSpec extends ZIOSpecDefault{
    def spec = suite("TapirSpec")(
    suite("NonEmptyString")(
      test("schema validation success") {
        val v   = NonEmptyString("hello")
        val validated = summon[Schema[NonEmptyString]].validator(v)
        assertTrue(validated == Nil)
      },
      test("codec parse success") {
        val decoded = summon[Codec.PlainCodec[NonEmptyString]].decode("hello")
        assertTrue(decoded == DecodeResult.Value(NonEmptyString("hello")))
      },
      test("schema validation failure") {
        val v   = NonEmptyString.unsafeWrapF[Id]("")
        val validated = summon[Schema[NonEmptyString]].validator(v)
        assertTrue(
            validated.exists(e => 
                e.validator.show == Some("String must not be empty") &&
                e.invalidValue  == ""
            )
        )
      },
      test("codec parse failure") {
        val decoded = summon[Codec.PlainCodec[NonEmptyString]].decode("")
        assert(decoded)(isSubtype[DecodeResult.Error](hasField("message", _.error.getMessage, equalTo("String must not be empty"))))
      }
    ),
    suite("SubtypeLongString")(
      test("schema validation success") {
        val v   = SubtypeLongString("hello world")
        val validated = summon[Schema[SubtypeLongString]].validator(v)
        assertTrue(validated == Nil)
      },
      test("codec parse success") {
        val decoded = summon[Codec.PlainCodec[SubtypeLongString]].decode("hello world")
        assertTrue(decoded == DecodeResult.Value(NonEmptyString("hello world")))
      },
      test("schema validation failure") {
        val v   = SubtypeLongString.unsafeWrapF[Id]("short")
        val validated = summon[Schema[SubtypeLongString]].validator(v)
        assertTrue(
            validated.exists(e => 
                e.validator.show == Some("String must be longer than 10 characters") &&
                e.invalidValue  == "short"
            )
        )
      },
      test("codec parse failure") {
        val decoded = summon[Codec.PlainCodec[SubtypeLongString]].decode("short")
        assert(decoded)(isSubtype[DecodeResult.Error](hasField("message", _.error.getMessage, equalTo("String must be longer than 10 characters"))))
      }
    ),
    suite("SimpleNewtype")(
      test("schema validation success") {
        val v   = SimpleNewtype(1)
        val validated = summon[Schema[SimpleNewtype]].validator(v)
        assertTrue(validated == Nil)
      },
      test("codec parse success") {
        val decoded = summon[Codec.PlainCodec[SimpleNewtype]].decode("1")
        assertTrue(decoded == DecodeResult.Value(SimpleNewtype(1)))
      },
      test("codec parse failure") {
        val decoded = summon[Codec.PlainCodec[SimpleNewtype]].decode("nope")
        assert(decoded)(isSubtype[DecodeResult.Error](hasField("error type", _.error, isSubtype[NumberFormatException](anything))))
      }
    ),
    suite("SimpleSubType")(
      test("schema validation success") {
        val v   = SimpleSubtype(1)
        val validated = summon[Schema[SimpleSubtype]].validator(v)
        assertTrue(validated == Nil)
      },
      test("codec parse success") {
        val decoded = summon[Codec.PlainCodec[SimpleSubtype]].decode("1")
        assertTrue(decoded == DecodeResult.Value(SimpleSubtype(1)))
      },
      test("codec parse failure") {
        val decoded = summon[Codec.PlainCodec[SimpleSubtype]].decode("nope")
        assert(decoded)(isSubtype[DecodeResult.Error](hasField("error type", _.error, isSubtype[NumberFormatException](anything))))
      }
    )
  )
}