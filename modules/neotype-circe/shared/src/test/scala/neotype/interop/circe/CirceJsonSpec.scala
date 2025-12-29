package neotype.interop.circe

import io.circe.*
import io.circe.syntax.*
import neotype.interop.circe.given
import neotype.test.*
import neotype.test.definitions.*

// Circe doesn't have a unified Codec type that's commonly used,
// so we create a stub combining Decoder and Encoder
final case class CirceCodec[A](decoder: Decoder[A], encoder: Encoder[A])
object CirceCodec:
  given [A](using d: Decoder[A], e: Encoder[A]): CirceCodec[A] = CirceCodec(d, e)

object CirceLibrary extends JsonLibrary[CirceCodec]:
  def decode[A](json: String)(using codec: CirceCodec[A]): Either[String, A] =
    given Decoder[A] = codec.decoder
    io.circe.parser.decode[A](json).left.map(_.getMessage)

  def encode[A](value: A)(using codec: CirceCodec[A]): String =
    given Encoder[A] = codec.encoder
    value.asJson.noSpaces

// Manual codec definitions (circe-generic not available)
given Decoder[Composite] = Decoder.instance { c =>
  for
    newtype       <- c.get[ValidatedNewtype]("newtype")
    simpleNewtype <- c.get[SimpleNewtype]("simpleNewtype")
    subtype       <- c.get[ValidatedSubtype]("subtype")
    simpleSubtype <- c.get[SimpleSubtype]("simpleSubtype")
  yield Composite(newtype, simpleNewtype, subtype, simpleSubtype)
}

given Encoder[Composite] = Encoder.instance { c =>
  Json.obj(
    "newtype"       -> c.newtype.asJson,
    "simpleNewtype" -> c.simpleNewtype.asJson,
    "subtype"       -> c.subtype.asJson,
    "simpleSubtype" -> c.simpleSubtype.asJson
  )
}

given Decoder[OptionalHolder] = Decoder.instance { c =>
  c.get[OptionalString]("value").map(OptionalHolder.apply)
}

given Encoder[OptionalHolder] = Encoder.instance { h =>
  Json.obj("value" -> h.value.asJson)
}

given Decoder[ListHolder] = Decoder.instance { c =>
  c.get[List[ValidatedNewtype]]("items").map(ListHolder.apply)
}

given Encoder[ListHolder] = Encoder.instance { h =>
  Json.obj("items" -> h.items.asJson)
}

object CirceJsonSpec extends JsonLibrarySpec[CirceCodec]("Circe", CirceLibrary):
  override protected def optionalHolderCodec: Option[CirceCodec[OptionalHolder]] =
    Some(summon[CirceCodec[OptionalHolder]])

  override protected def listHolderCodec: Option[CirceCodec[ListHolder]] =
    Some(summon[CirceCodec[ListHolder]])
