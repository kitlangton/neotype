package neotype.ziojson

import neotype.*
import neotype.test.*
import neotype.test.definitions.*
import zio.json.*
import zio.test.*

object ZioJsonLibrary extends JsonLibrary[JsonCodec]:
  def decode[A](json: String)(using codec: JsonCodec[A]): Either[String, A] = json.fromJson[A]
  def encode[A](value: A)(using codec: JsonCodec[A]): String                = value.toJson

given compositeCodec: JsonCodec[Composite] = DeriveJsonCodec.gen[Composite]
object ZioJsonSpec extends JsonLibrarySpec[JsonCodec]("ZioJson", ZioJsonLibrary)
