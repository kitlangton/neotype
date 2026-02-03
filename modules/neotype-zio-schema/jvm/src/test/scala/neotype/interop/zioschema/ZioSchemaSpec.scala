package neotype.interop.zioschema

import neotype.interop.zioschema.given_Schema_B
import neotype.test.definitions.*
import zio.*
import zio.json.JsonEncoder
import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.DecodeError
import zio.schema.codec.DecodeError.ReadError
import zio.schema.codec.JsonCodec
import zio.stream.ZStream
import zio.test.*

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets

object ZioSchemaSpec extends ZIOSpecDefault:

  given Schema[Composite]           = DeriveSchema.gen[Composite]
  given Schema[CompositeUnderlying] = DeriveSchema.gen[CompositeUnderlying]

  def spec = suite("zioSchemaSpec")(
    suite("NonEmptyString")(
      test("parse success") {
        val json = JsonEncoder.string.encodeJson("hello", None)
        decode[ValidatedNewtype](json).map { result =>
          assertTrue(result == ValidatedNewtype("hello"))
        }
      },
      test("parse failure") {
        val json = JsonEncoder.string.encodeJson("", None)
        decode[ValidatedNewtype](json).exit.map { result =>
          assertTrue(result.is(_.failure) == readError("(String must not be empty)"))
        }
      }
    ),
    suite("SubtypeLongString")(
      test("parse success") {
        val json = JsonEncoder.string.encodeJson("hello world", None)
        decode[ValidatedSubtype](json).map { result =>
          assertTrue(result == ValidatedSubtype("hello world"))
        }
      },
      test("parse failure") {
        val json = JsonEncoder.string.encodeJson("hello", None)
        decode[ValidatedSubtype](json).exit.map { result =>
          assertTrue(result.is(_.failure) == readError("(String must be longer than 10 characters)"))
        }
      }
    ),
    suite("SimpleNewType")(
      test("parse success") {
        val json = JsonEncoder.string.encodeJson("123", None)
        decode[SimpleNewtype](json).map { result =>
          assertTrue(result == SimpleNewtype(123))
        }
      },
      test("parse failure") {
        val json: CharSequence = JsonEncoder.string.encodeJson("hello", None)
        decode[SimpleNewtype](json).exit.map { result =>
          assertTrue(result.is(_.failure) == readError("(expected an Int)"))
        }
      }
    ),
    suite("Composite")(
      test("parse success") {
        val json = encode[CompositeUnderlying](CompositeUnderlying("hello", 123, "hello world", 123))
        decode[Composite](json).map { result =>
          assertTrue(
            result == Composite(
              ValidatedNewtype("hello"),
              SimpleNewtype(123),
              ValidatedSubtype("hello world"),
              SimpleSubtype(123)
            )
          )
        }
      },
      test("parse failure") {
        val json = encode[CompositeUnderlying](CompositeUnderlying("", 123, "hello", 123))
        decode[Composite](json).exit.map { result =>
          assertTrue(result.is(_.failure) == readError(".newtype(String must not be empty)"))
        }
      }
    )
  )

private def readError(message: String) = ReadError(Cause.Empty, message)

private def decode[A](json: CharSequence)(using schema: Schema[A]): ZIO[Any, DecodeError, A] =
  val bytes = StandardCharsets.UTF_8.newEncoder().encode(CharBuffer.wrap(json))

  ZIO.absolve {
    ZStream
      .fromChunk(Chunk.fromByteBuffer(bytes))
      .runCollect
      .map { b =>
        JsonCodec.schemaBasedBinaryCodec[A](schema).decode(b)
      }
  }

private def encode[A](a: A)(using schema: Schema[A]): String =
  val bytes = JsonCodec.schemaBasedBinaryCodec[A](schema).encode(a)
  String(bytes.toArray, StandardCharsets.UTF_8)
