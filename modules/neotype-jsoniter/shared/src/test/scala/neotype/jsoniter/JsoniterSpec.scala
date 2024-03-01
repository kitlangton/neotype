package neotype.jsoniter

import neotype.*
import zio.test.*
import zio.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonCodec, JsonValueCodec, readFromString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import neotype.jsoniter.given

import scala.util.Try
import neotype.test.definitions.*

object JsoniterSpec extends ZIOSpecDefault:

  given JsonValueCodec[Composite] = JsonCodecMaker.make

  def spec = suite("Jsoniter Support")(
    test("parse success") {
      val json =
        """{
          |  "newtype": "hello",
          |  "simpleNewtype": 1,
          |  "subtype": "hello world",
          |  "simpleSubtype": 1
          |}""".stripMargin
      val result = readFromString[Composite](json)
      assertTrue(
        result == Composite(
          ValidatedNewtype("hello"),
          SimpleNewtype(1),
          ValidatedSubtype("hello world"),
          SimpleSubtype(1)
        )
      )
    },
    test("failure on nonEmptyString") {
      val json =
        """{
          |  "newtype": "",
          |  "simpleNewtype": 1,
          |  "subtype": "hello world",
          |  "simpleSubtype": 1
          |}""".stripMargin
      val result = Try(readFromString[Composite](json))
      assertTrue(result.toEither.is(_.left).getMessage contains "String must not be empty")
    },
    test("failure on subtypeLongString") {
      val json =
        """{
          |  "newtype": "a",
          |  "simpleNewtype": 1,
          |  "subtype": "hello-",
          |  "simpleSubtype": 1
          |}""".stripMargin
      val result = Try(readFromString[Composite](json))
      assertTrue(result.toEither.is(_.left).getMessage contains "String must be longer than 10 characters")
    }
  )
