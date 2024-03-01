package neotype.jsoniter

import neotype.*
import zio.test.*
import zio.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonCodec, JsonValueCodec, readFromString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import neotype.jsoniter.given

import scala.util.Try

type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  override inline def validate(value: String): Boolean =
    value.nonEmpty

  override inline def failureMessage = "String must not be empty"

type SubtypeLongString = SubtypeLongString.Type
given SubtypeLongString: Subtype[String] with
  override inline def validate(value: String): Boolean =
    value.length > 10

  override inline def failureMessage = "String must be longer than 10 characters"

type SimpleNewtype = SimpleNewtype.Type
given SimpleNewtype: Newtype[Int]()

type SimpleSubtype = SimpleSubtype.Type
given SimpleSubtype: Subtype[String]()

object JsoniterSpec extends ZIOSpecDefault:
  final case class Composed(
      nonEmptyString: NonEmptyString,
      subtypeLongString: SubtypeLongString,
      simpleNewtype: SimpleNewtype,
      simpleSubtype: SimpleSubtype
  )

  object Composed:
    given JsonValueCodec[Composed] =
      JsonCodecMaker.make[Composed]

  def spec = suite("Jsoniter Support")(
    test("parse success") {
      val json =
        """{
          |  "nonEmptyString": "a",
          |  "subtypeLongString": "12345678901",
          |  "simpleNewtype": 1,
          |  "simpleSubtype": "simple"
          |}""".stripMargin
      val result = readFromString[Composed](json)
      assertTrue(
        result == Composed(
          NonEmptyString("a"),
          SubtypeLongString("12345678901"),
          SimpleNewtype(1),
          SimpleSubtype("simple")
        )
      )
    },
    test("failure on nonEmptyString") {
      val json =
        """{
          |  "nonEmptyString": "",
          |  "subtypeLongString": "01234567890",
          |  "simpleNewtype": 1,
          |  "simpleSubtype": "simple"
          |}""".stripMargin
      val result = Try(readFromString[Composed](json))
      assertTrue(result.toEither.is(_.left).getMessage contains "String must not be empty")
    },
    test("failure on subtypeLongString") {
      val json =
        """{
          |  "nonEmptyString": "a",
          |  "subtypeLongString": "0123456789",
          |  "simpleNewtype": 1,
          |  "simpleSubtype": "simple"
          |}""".stripMargin
      val result = Try(readFromString[Composed](json))
      assertTrue(result.toEither.is(_.left).getMessage contains "String must be longer than 10 characters")
    }
  )
