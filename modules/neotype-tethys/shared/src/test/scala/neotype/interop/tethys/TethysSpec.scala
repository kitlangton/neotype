package neotype.interop.pureconfig

import neotype.interop.tethys.given
import neotype.test.JsonLibrary
import neotype.test.JsonLibrarySpec
import neotype.test.definitions.*
import tethys.*
import tethys.jackson.*
import zio.test.ZIOSpecDefault

// no common native codec is provided by tethys
final private case class TethysCodecStub[A](val reader: JsonReader[A], val writer: JsonWriter[A])
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

object TethysSpec extends JsonLibrarySpec[TethysCodecStub]("Tethys", TethysLibrary)
