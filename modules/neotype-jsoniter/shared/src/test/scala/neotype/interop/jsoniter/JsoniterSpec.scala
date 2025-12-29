package neotype.interop.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import com.github.plokhotnyuk.jsoniter_scala.macros.ConfiguredJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import neotype.interop.jsoniter.given
import neotype.test.*
import neotype.test.EmptyListEncoding
import neotype.test.definitions.*
import zio.test.*

import scala.util.Try

object JsoniterLibrary extends JsonLibrary[JsonValueCodec]:
  def decode[A](json: String)(using codec: JsonValueCodec[A]): Either[String, A] =
    Try(readFromString[A](json)).toEither.left.map(_.getMessage)

  def encode[A](value: A)(using codec: JsonValueCodec[A]): String =
    writeToString(value)

// Only define composite codecs - individual newtypes use the implicit neotypeCodec
given JsonValueCodec[Composite]      = JsonCodecMaker.make
given JsonValueCodec[OptionalHolder] = JsonCodecMaker.make
given JsonValueCodec[ListHolder]     = JsonCodecMaker.make

object JsoniterSpec extends JsonLibrarySpec[JsonValueCodec]("Jsoniter", JsoniterLibrary):
  override protected def optionalHolderCodec: Option[JsonValueCodec[OptionalHolder]] =
    Some(summon[JsonValueCodec[OptionalHolder]])
  override protected def listHolderCodec: Option[JsonValueCodec[ListHolder]] =
    Some(summon[JsonValueCodec[ListHolder]])
  override protected def emptyListEncoding: EmptyListEncoding = EmptyListEncoding.OmitField

  override protected def additionalSuites: List[Spec[Any, Nothing]] = List(
    suite("CodecMakerConfig")(
      test("use codec maker configuration with stringified numbers") {
        inline given CodecMakerConfig   = CodecMakerConfig.withIsStringified(true)
        given JsonValueCodec[Composite] = ConfiguredJsonValueCodec.derived

        val json =
          """{
            |  "newtype": "hello",
            |  "simpleNewtype": "123",
            |  "subtype": "hello world",
            |  "simpleSubtype": "123"
            |}""".stripMargin
        val result = readFromString[Composite](json)
        assertTrue(
          result == Composite(
            ValidatedNewtype("hello"),
            SimpleNewtype(123),
            ValidatedSubtype("hello world"),
            SimpleSubtype(123)
          )
        )
      }
    )
  )
