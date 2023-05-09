package neotype.zioschema

import neotype.{Newtype, Subtype}
import zio.*
import zio.schema.codec.DecodeError.ReadError
import zio.schema.codec.JsonCodec.JsonDecoder
import zio.schema.codec.JsonCodec.JsonEncoder.charSequenceToByteChunk
import zio.schema.codec.{DecodeError, JsonCodec}
import zio.schema.{DeriveSchema, Schema}
import zio.stream.ZStream
import zio.test.*
import zio.test.Assertion.*
import zio.json.{DeriveJsonEncoder, JsonEncoder}
import zio.json.*

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
final case class Person2NoTypeChecking(name: String, age: Int, address: String)

object Person2NoTypeChecking:
  val encoder: JsonEncoder[Person2NoTypeChecking] = DeriveJsonEncoder.gen

object ZioSchemaSpec extends ZIOSpecDefault:
  def spec = suite("zioSchemaSpec")(
    suite("NonEmptyString")(
      test("parse success") {
        val json = JsonEncoder.string.encodeJson("hello", None)
        assertZIO(decode(Schema[NonEmptyString], json))(equalTo(NonEmptyString("hello")))
      },
      test("parse failure") {
        val json = JsonEncoder.string.encodeJson("", None)
        assertZIO(decode(Schema[NonEmptyString], json).exit)(
          fails(equalTo(readError("(String must not be empty)")))
        )
      }
    ),
    suite("SubtypeLongString")(
      test("parse success") {
        val json = JsonEncoder.string.encodeJson("hello world", None)
        assertZIO(decode(Schema[SubtypeLongString], json))(equalTo(SubtypeLongString("hello world")))
      },
      test("parse failure") {
        val json = JsonEncoder.string.encodeJson("hello", None)
        assertZIO(decode(Schema[SubtypeLongString], json).exit)(
          fails(equalTo(readError("(String must be longer than 10 characters)")))
        )
      }
    ),
    suite("SimpleNewType")(
      test("parse success") {
        val json = JsonEncoder.string.encodeJson("123", None)
        assertZIO(decode(Schema[SimpleNewtype], json))(equalTo(SimpleNewtype(123)))
      },
      test("parse failure") {
        val json: CharSequence = JsonEncoder.string.encodeJson("hello", None)
        assertZIO(decode(Schema[SimpleNewtype], json).exit)(
          fails(equalTo(readError("(expected a number, got h)")))
        )
      }
    ),
    suite("Person")(
      test("parse success") {
        val json   = Person2NoTypeChecking.encoder.encodeJson(Person2NoTypeChecking("hello", 123, "hello world"), None)
        val person = Person(NonEmptyString("hello"), 123, SubtypeLongString("hello world"))

        assertZIO(decode(DeriveSchema.gen[Person], json))(
          equalTo(person)
        )
      },
      test("parse failure") {
        val json = Person2NoTypeChecking.encoder.encodeJson(Person2NoTypeChecking("", 123, "hello world"), None)

        assertZIO(decode(DeriveSchema.gen[Person], json).exit)(
          fails(equalTo(readError(".name(String must not be empty)")))
        )
      }
    )
  )

private def readError(message: String) = ReadError(Cause.Empty, message)

private def decode[A](schema: Schema[A], json: CharSequence): ZIO[Any, DecodeError, A] =
  val bytes = StandardCharsets.UTF_8.newEncoder().encode(CharBuffer.wrap(json))

  ZIO.absolve {
    ZStream
      .fromChunk(Chunk.fromByteBuffer(bytes))
      .runCollect
      .map { b =>
        JsonCodec.schemaBasedBinaryCodec[A](schema).decode(b)
      }
  }
