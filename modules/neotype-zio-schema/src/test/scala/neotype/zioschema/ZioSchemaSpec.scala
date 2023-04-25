package neotype.zioschema

import neotype.{Newtype, Subtype}
import zio.json.JsonDecoder.JsonError
import zio.json.{DeriveJsonEncoder, JsonEncoder}
import zio.schema.codec.DecodeError.ReadError
import zio.schema.codec.JsonCodec.JsonDecoder
import zio.schema.codec.JsonCodec.JsonEncoder.CHARSET
import zio.schema.codec.{DecodeError, JsonCodec}
import zio.schema.{DeriveSchema, Schema}
import zio.stream.ZStream
import zio.test.*
import zio.test.Assertion.*
import zio.{Cause, Chunk}

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets

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
given SimpleSubtype: Subtype.Simple[String]()

final case class Person(name: NonEmptyString, age: Int, address: SubtypeLongString)

object ZioSchemaSpec extends ZIOSpecDefault:
  def spec = suite("zioSchemaSpec")(
    suite("NonEmptyString")(
      test("parse success") {
        assertDecodes(Schema[NonEmptyString], NonEmptyString("hello"), stringify("hello"))
      },
      test("parse failure") {
        assertDecodesToError(
          Schema[NonEmptyString],
          JsonEncoder.string.encodeJson("", None),
          JsonError.Message("String must not be empty") :: Nil
        )
      }
    ),
    suite("SubtypeLongString")(
      test("parse success") {
        assertDecodes(Schema[SubtypeLongString], SubtypeLongString("hello world"), stringify("hello world"))
      },
      test("parse failure") {
        assertDecodesToError(
          Schema[SubtypeLongString],
          JsonEncoder.string.encodeJson("hello", None),
          JsonError.Message("String must be longer than 10 characters") :: Nil
        )
      }
    ),
    suite("SimpleNewType")(
      test("parse success") {
        assertDecodes(Schema[SimpleNewtype], SimpleNewtype(123), stringify("123"))
      },
      test("parse failure") {
        assertDecodesToError(
          Schema[SimpleNewtype],
          JsonEncoder.string.encodeJson("hello", None),
          JsonError.Message("expected a number, got h") :: Nil
        )
      }
    ),
    suite("Person")(
      test("parse success") {
        val json = """ { "name": "hello", "age": 123, "address": "hello world" } """
        assertDecodes(
          DeriveSchema.gen[Person],
          Person(NonEmptyString("hello"), 123, SubtypeLongString("hello world")),
          charSequenceToByteChunk(json)
        )
      },
      test("parse failure") {
        val json = """{"name":"","age":10,"address":"hello world"}"""
        assertDecodesToError(
          DeriveSchema.gen[Person],
          JsonEncoder.string.encodeJson(json, None),
          JsonError.Message(".name(String must not be empty)") :: Nil
        )
      }
    )
  )

private def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] =
  val bytes = StandardCharsets.UTF_8.newEncoder().encode(CharBuffer.wrap(chars))
  Chunk.fromByteBuffer(bytes)

private def assertDecodesToError[A](schema: Schema[A], json: CharSequence, errors: List[JsonError]) =
  val stream = ZStream
    .fromChunk(charSequenceToByteChunk(json))
    .via(JsonCodec.schemaBasedBinaryCodec[A](schema).streamDecoder)
    .catchAll(ZStream.succeed[DecodeError](_))
    .runHead
  assertZIO(stream)(isSome(equalTo(ReadError(Cause.empty, JsonError.render(errors)))))

private def stringify(s: String): Chunk[Byte] =
  val encoded = JsonEncoder.string.encodeJson(s, None)
  charSequenceToByteChunk(encoded)

private def assertDecodes[A](schema: Schema[A], value: A, chunk: Chunk[Byte]) =
  val result = ZStream.fromChunk(chunk).via(JsonCodec.schemaBasedBinaryCodec[A](schema).streamDecoder).runCollect
  assertZIO(result)(equalTo(Chunk(value)))
