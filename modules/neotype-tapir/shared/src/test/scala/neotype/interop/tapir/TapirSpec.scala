package neotype.interop.tapir

import neotype.interop.tapir.given_Codec_L_B_CF
import neotype.interop.tapir.given_Pickler_B
import neotype.interop.tapir.given_Schema_B
import neotype.test.definitions.*
import sttp.tapir.Codec
import sttp.tapir.DecodeResult
import sttp.tapir.DecodeResult.Error.JsonDecodeException
import sttp.tapir.Schema
import sttp.tapir.json.pickler.Pickler
import zio.test.*
import zio.test.Assertion.*

object TapirSpec extends ZIOSpecDefault:

  given Pickler[Composite] = Pickler.derived

  def spec =
    suite("TapirSpec")(
      suite("ValidatedNewtype")(
        test("pickler validation success") {
          val v         = ValidatedNewtype("hello")
          val pickler   = summon[Pickler[ValidatedNewtype]]
          val validated = pickler.schema.validator(v)
          val codec     = pickler.toCodec
          val decoded   = codec.decode("\"hello\"")
          assertTrue(
            validated == Nil,
            decoded == DecodeResult.Value(ValidatedNewtype("hello"))
          )
        },
        test("schema validation success") {
          val v         = ValidatedNewtype("hello")
          val validated = summon[Schema[ValidatedNewtype]].validator(v)
          assertTrue(validated == Nil)
        },
        test("codec parse success") {
          val decoded = summon[Codec.PlainCodec[ValidatedNewtype]].decode("hello")
          assertTrue(decoded == DecodeResult.Value(ValidatedNewtype("hello")))
        },
        test("pickler validation failure") {
          val v         = ValidatedNewtype.unsafeMake("")
          val pickler   = summon[Pickler[ValidatedNewtype]]
          val validated = pickler.schema.validator(v)
          val codec     = pickler.toCodec
          val decoded   = codec.decode("\"\"")
          assertTrue(
            validated.exists(e =>
              e.customMessage == Some("String must not be empty") &&
                e.invalidValue == ""
            ),
            decoded.is(_.subtype[DecodeResult.Error]).original == "\"\"",
            decoded
              .is(_.subtype[DecodeResult.Error])
              .error
              .is(_.subtype[JsonDecodeException])
              .underlying
              .getMessage == "String must not be empty"
          )
        },
        test("schema validation failure") {
          val v         = ValidatedNewtype.unsafeMake("")
          val validated = summon[Schema[ValidatedNewtype]].validator(v)
          assertTrue(
            validated.exists(e =>
              e.customMessage == Some("String must not be empty") &&
                e.invalidValue == ""
            )
          )
        },
        test("codec parse failure") {
          val decoded = summon[Codec.PlainCodec[ValidatedNewtype]].decode("")
          assert(decoded)(
            isSubtype[DecodeResult.Error](hasField("message", _.error.getMessage, equalTo("String must not be empty")))
          )
        }
      ),
      suite("Composite")(
        test("schema validation success") {
          val v = Composite(
            ValidatedNewtype("hello"),
            SimpleNewtype(1),
            ValidatedSubtype("hello world"),
            SimpleSubtype(1)
          )
          val validated = summon[Pickler[Composite]].schema.validator(v)
          assertTrue(validated == Nil)
        },
        test("pickler validation success") {
          val v = Composite(
            ValidatedNewtype("hello"),
            SimpleNewtype(1),
            ValidatedSubtype("hello world"),
            SimpleSubtype(1)
          )
          val pickler   = summon[Pickler[Composite]]
          val validated = pickler.schema.validator(v)
          val codec     = pickler.toCodec
          val decoded = codec.decode(
            """{"newtype":"hello","subtype":"hello world","simpleNewtype":1,"simpleSubtype":1}"""
          )
          assertTrue(
            validated == Nil,
            decoded == DecodeResult.Value(
              Composite(
                ValidatedNewtype("hello"),
                SimpleNewtype(1),
                ValidatedSubtype("hello world"),
                SimpleSubtype(1)
              )
            )
          )
        },
        test("codec parse failure") {
          val decoded = summon[Pickler[Composite]].toCodec.decode(
            """{"newtype":"","subtype":"short","simpleNewtype":1,"simpleSubtype":1}"""
          )
          assertTrue(
            decoded
              .is(_.subtype[DecodeResult.Error])
              .error
              .is(_.subtype[JsonDecodeException])
              .underlying
              .getMessage == "String must not be empty"
          )
        }
      ),
      suite("ValidatedSubtype")(
        test("schema validation success") {
          val v         = ValidatedSubtype("hello world")
          val validated = summon[Schema[ValidatedSubtype]].validator(v)
          assertTrue(validated == Nil)
        },
        test("codec parse success") {
          val decoded = summon[Codec.PlainCodec[ValidatedSubtype]].decode("hello world")
          assertTrue(decoded == DecodeResult.Value(ValidatedNewtype("hello world")))
        },
        test("schema validation failure") {
          val v         = ValidatedSubtype.unsafeMake("short")
          val validated = summon[Schema[ValidatedSubtype]].validator(v)
          assertTrue(
            validated.exists(e =>
              e.customMessage == Some("String must be longer than 10 characters") &&
                e.invalidValue == "short"
            )
          )
        },
        test("codec parse failure") {
          val decoded = summon[Codec.PlainCodec[ValidatedSubtype]].decode("short")
          assert(decoded)(
            isSubtype[DecodeResult.Error](
              hasField("message", _.error.getMessage, equalTo("String must be longer than 10 characters"))
            )
          )
        }
      ),
      suite("SimpleNewtype")(
        test("schema validation success") {
          val v         = SimpleNewtype(1)
          val validated = summon[Schema[SimpleNewtype]].validator(v)
          assertTrue(validated == Nil)
        },
        test("codec parse success") {
          val decoded = summon[Codec.PlainCodec[SimpleNewtype]].decode("1")
          assertTrue(decoded == DecodeResult.Value(SimpleNewtype(1)))
        },
        test("codec parse failure") {
          val decoded = summon[Codec.PlainCodec[SimpleNewtype]].decode("nope")
          assert(decoded)(
            isSubtype[DecodeResult.Error](hasField("error type", _.error, isSubtype[NumberFormatException](anything)))
          )
        },
        test("pickler validation success") {
          val v         = SimpleNewtype(1)
          val pickler   = summon[Pickler[SimpleNewtype]]
          val validated = pickler.schema.validator(v)
          val codec     = pickler.toCodec
          val decoded   = codec.decode("1")
          assertTrue(
            validated == Nil,
            decoded == DecodeResult.Value(SimpleNewtype(1))
          )
        },
        test("pickler validation failure") {
          val decoded = summon[Pickler[SimpleNewtype]].toCodec.decode("true")
          assertTrue(
            decoded
              .is(_.subtype[DecodeResult.Error])
              .error
              .is(_.subtype[JsonDecodeException])
              .underlying
              .getMessage == "expected number got boolean at index 0"
          )
        }
      ),
      suite("SimpleSubType")(
        test("schema validation success") {
          val v         = SimpleSubtype(1)
          val validated = summon[Schema[SimpleSubtype]].validator(v)
          assertTrue(validated == Nil)
        },
        test("codec parse success") {
          val decoded = summon[Codec.PlainCodec[SimpleSubtype]].decode("1")
          assertTrue(decoded == DecodeResult.Value(SimpleSubtype(1)))
        },
        test("codec parse failure") {
          val decoded = summon[Codec.PlainCodec[SimpleSubtype]].decode("nope")
          assert(decoded)(
            isSubtype[DecodeResult.Error](hasField("error type", _.error, isSubtype[NumberFormatException](anything)))
          )
        }
      )
    )
