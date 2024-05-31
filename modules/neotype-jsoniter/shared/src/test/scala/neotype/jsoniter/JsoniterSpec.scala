package neotype.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import com.github.plokhotnyuk.jsoniter_scala.macros.ConfiguredJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import neotype.*
import neotype.jsoniter.given
import neotype.test.definitions.*
import zio.*
import zio.test.*

import scala.util.Try

object JsoniterSpec extends ZIOSpecDefault:

  def spec = suite("Jsoniter Support")(
    test("parse success") {
      given JsonValueCodec[Composite] = JsonCodecMaker.make

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
    test("use codec maker configuration") {
      inline given CodecMakerConfig   = CodecMakerConfig.withIsStringified(true)
      given JsonValueCodec[Composite] = ConfiguredJsonValueCodec.derived

      val json =
        """{
          |  "newtype": "hello",
          |  "simpleNewtype": "1",
          |  "subtype": "hello world",
          |  "simpleSubtype": "1"
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
      given JsonValueCodec[Composite] = JsonCodecMaker.make

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
      given JsonValueCodec[Composite] = JsonCodecMaker.make

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
