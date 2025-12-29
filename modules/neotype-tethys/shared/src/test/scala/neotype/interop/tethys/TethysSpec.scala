package neotype.interop.tethys

import neotype.interop.tethys.given
import neotype.test.JsonLibrary
import neotype.test.JsonLibrarySpec
import neotype.test.NoneEncoding
import neotype.test.definitions.*
import tethys.*
import tethys.jackson.*

// no common native codec is provided by tethys
final case class TethysCodecStub[A](val reader: JsonReader[A], val writer: JsonWriter[A])
object TethysCodecStub:
  given [A](using reader: JsonReader[A], writer: JsonWriter[A]): TethysCodecStub[A] =
    TethysCodecStub(reader, writer)

object TethysLibrary extends JsonLibrary[TethysCodecStub]:
  override def decode[A](json: String)(using codec: TethysCodecStub[A]): Either[String, A] =
    given JsonReader[A] = codec.reader
    json.jsonAs[A].left.map(_.getMessage)

  override def encode[A](value: A)(using codec: TethysCodecStub[A]): String =
    given JsonWriter[A] = codec.writer
    value.asJson

given JsonReader[Composite] =
  JsonReader.builder
    .addField[ValidatedNewtype]("newtype")
    .addField[SimpleNewtype]("simpleNewtype")
    .addField[ValidatedSubtype]("subtype")
    .addField[SimpleSubtype]("simpleSubtype")
    .buildReader(Composite.apply)

given JsonWriter[Composite] =
  JsonWriter
    .obj[Composite]
    .addField("newtype")(_.newtype)
    .addField("simpleNewtype")(_.simpleNewtype)
    .addField("subtype")(_.subtype)
    .addField("simpleSubtype")(_.simpleSubtype)

given JsonReader[OptionalHolder] =
  JsonReader.builder
    .addField[OptionalString]("value")
    .buildReader(OptionalHolder.apply)

given JsonWriter[OptionalHolder] =
  JsonWriter
    .obj[OptionalHolder]
    .addField("value")(_.value)

given JsonReader[ListHolder] =
  JsonReader.builder
    .addField[List[ValidatedNewtype]]("items")
    .buildReader(ListHolder.apply)

given JsonWriter[ListHolder] =
  JsonWriter
    .obj[ListHolder]
    .addField("items")(_.items)

object TethysSpec extends JsonLibrarySpec[TethysCodecStub]("Tethys", TethysLibrary):
  override protected def optionalHolderCodec: Option[TethysCodecStub[OptionalHolder]] =
    Some(summon[TethysCodecStub[OptionalHolder]])
  override protected def listHolderCodec: Option[TethysCodecStub[ListHolder]] =
    Some(summon[TethysCodecStub[ListHolder]])
  override protected def noneEncoding: NoneEncoding = NoneEncoding.OmitField
