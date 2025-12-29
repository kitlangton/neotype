package neotype.interop.ziojson

import neotype.*
import neotype.interop.ziojson.given
import neotype.test.*
import neotype.test.NoneEncoding
import neotype.test.definitions.*
import zio.json.*
import zio.test.*

object ZioJsonLibrary extends JsonLibrary[JsonCodec]:
  def decode[A](json: String)(using codec: JsonCodec[A]): Either[String, A] = json.fromJson[A]
  def encode[A](value: A)(using codec: JsonCodec[A]): String                = value.toJson

given JsonCodec[Composite]      = DeriveJsonCodec.gen[Composite]
given JsonCodec[OptionalHolder] = DeriveJsonCodec.gen[OptionalHolder]
given JsonCodec[ListHolder]     = DeriveJsonCodec.gen[ListHolder]

object ZioJsonSpec extends JsonLibrarySpec[JsonCodec]("ZioJson", ZioJsonLibrary):
  override protected def optionalHolderCodec: Option[JsonCodec[OptionalHolder]] =
    Some(summon[JsonCodec[OptionalHolder]])
  override protected def listHolderCodec: Option[JsonCodec[ListHolder]] =
    Some(summon[JsonCodec[ListHolder]])
  override protected def noneEncoding: NoneEncoding = NoneEncoding.OmitField
