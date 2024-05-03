package neotype.test

import neotype.test.definitions.*
import zio.test.*

trait JsonLibrary[Codec[_]]:
  def decode[A](json: String)(using codec: Codec[A]): Either[String, A]
  def encode[A](value: A)(using codec: Codec[A]): String

trait JsonLibrarySpec[Codec[_]](
    name: String,
    val library: JsonLibrary[Codec]
)(using
    compositeCodec: Codec[Composite],
    validatedNewtypeCodec: Codec[ValidatedNewtype],
    validatedSubtypeCodec: Codec[ValidatedSubtype],
    simpleNewtypeCodec: Codec[SimpleNewtype],
    simpleSubtypeCodec: Codec[SimpleSubtype]
) extends ZIOSpecDefault:

  def spec = suite(s"${name}JsonSpec")(
    suite("NonEmptyString")(
      test("decode success") {
        val json   = """ "hello" """
        val parsed = library.decode[ValidatedNewtype](json)
        assertTrue(parsed == Right(ValidatedNewtype("hello")))
      },
      test("decode failure") {
        val json   = """ "" """
        val parsed = library.decode[ValidatedNewtype](json)
        assertTrue(parsed.is(_.left).contains("String must not be empty"))
      },
      test("encode") {
        val json = library.encode(ValidatedNewtype("hello"))
        assertTrue(json == """"hello"""")
      }
    ),
    suite("SubtypeLongString")(
      test("decode success") {
        val json   = """ "hello world" """
        val parsed = library.decode[ValidatedSubtype](json)
        assertTrue(parsed == Right(ValidatedSubtype("hello world")))
      },
      test("decode failure") {
        val json   = """ "hello" """
        val parsed = library.decode[ValidatedSubtype](json)
        assertTrue(parsed.is(_.left).contains("String must be longer than 10 characters"))
      },
      test("encode") {
        val json = library.encode(ValidatedSubtype("hello world"))
        assertTrue(json == """"hello world"""")
      }
    ),
    suite("SimpleNewtype")(
      test("decode success") {
        val json   = """ 123 """
        val parsed = library.decode[SimpleNewtype](json)
        assertTrue(parsed == Right(SimpleNewtype(123)))
      },
      test("decode failure") {
        val json   = """ "hello" """
        val parsed = library.decode[SimpleNewtype](json)
        assertTrue(parsed.isLeft)
      },
      test("encode") {
        val json = library.encode(SimpleNewtype(123))
        assertTrue(json == "123")
      }
    ),
    suite("SimpleSubtype")(
      test("decode success") {
        val json   = """ 123 """
        val parsed = library.decode[SimpleSubtype](json)
        assertTrue(parsed == Right(SimpleSubtype(123)))
      },
      test("decode failure") {
        val json   = """ "WHOOPS" """
        val parsed = library.decode[SimpleSubtype](json)
        assertTrue(parsed.isLeft)
      },
      test("encode") {
        val json = library.encode(SimpleSubtype(123))
        assertTrue(json == "123")
      }
    ),
    suite("Composite")(
      test("decode success") {
        val json =
          """ { "newtype": "hello", "simpleNewtype": 123, "subtype": "hello world", "simpleSubtype": 123 } """
        val parsed = library.decode[Composite](json)
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
      test("decode failure") {
        val json   = """ { "newtype": "", "simpleNewtype": 123, "subtype": "hello", "simpleSubtype": "WHOOPS" } """
        val parsed = library.decode[Composite](json)
        assertTrue(parsed.is(_.left).contains("String must not be empty"))
      },
      test("encode") {
        val json = library.encode(
          Composite(
            ValidatedNewtype("hello"),
            SimpleNewtype(123),
            ValidatedSubtype("hello world"),
            SimpleSubtype(123)
          )
        )
        assertTrue(
          json == """{"newtype":"hello","simpleNewtype":123,"subtype":"hello world","simpleSubtype":123}"""
        )
      }
    )
  )
