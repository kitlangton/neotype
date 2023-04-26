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
import zio.{Cause, Chunk, ZIO}

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
        assertZIO(decode(Schema[NonEmptyString], "hello"))(equalTo(NonEmptyString("hello")))
      },
      test("parse failure") {
        assertZIO(decode(Schema[NonEmptyString], "").exit)(
          fails(equalTo(readError("(String must not be empty)")))
        )
      }
    ),
    suite("SubtypeLongString")(
      test("parse success") {
        assertZIO(decode(Schema[SubtypeLongString], "hello world"))(equalTo(SubtypeLongString("hello world")))
      },
      test("parse failure") {
        assertZIO(decode(Schema[SubtypeLongString], "hello").exit)(
          fails(equalTo(readError("(String must be longer than 10 characters)")))
        )
      }
    ),
    suite("SimpleNewType")(
      test("parse success") {
        assertZIO(decode(Schema[SimpleNewtype], "123"))(equalTo(SimpleNewtype(123)))
      },
      test("parse failure") {
        assertZIO(decode(Schema[SimpleNewtype], "hello").exit)(
          fails(equalTo(readError("(expected a number, got h)")))
        )
      }
    ),
    suite("Person")(
      test("parse success") {
        val json   = """{"name":"hello","age":123,"address":"hello world"}"""
        val person = Person(NonEmptyString("hello"), 123, SubtypeLongString("hello world"))

        assertZIO(decode(DeriveSchema.gen[Person], json))(
          equalTo(person)
        )
      },
      test("parse failure") {
        val json = """{"name":"","age":10,"address":"hello"}"""
        assertZIO(decode(Schema[SimpleNewtype], json).exit)(
          fails(equalTo(readError(".name(String must not be empty)")))
        )
      }
    )
  )
private def readError(message: String) = ReadError(Cause.Empty, message)

private def decode[A](schema: Schema[A], json: String): ZIO[Any, DecodeError, A] =
  val encoded = JsonEncoder.string.encodeJson(json, None)
  val bytes   = StandardCharsets.UTF_8.newEncoder().encode(CharBuffer.wrap(encoded))
  val chunk   = Chunk.fromByteBuffer(bytes)

  ZIO.absolve {
    ZStream
      .fromChunk(chunk)
      .runCollect
      .map { (bytes: Chunk[Byte]) =>
        JsonCodec.schemaBasedBinaryCodec[A](schema).decode(bytes)
      }
  }
