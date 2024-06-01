package neotype.interop.playjson

import neotype.interop.playjson.newtypeFormat
import neotype.interop.playjson.simpleNewtypeFormat
import neotype.interop.playjson.simpleSubtypeFormat
import neotype.interop.playjson.subtypeFormat
import neotype.test.*
import neotype.test.definitions.*
import play.api.libs.json.*

object PlayJsonLibrary extends JsonLibrary[Format]:
  def decode[A](json: String)(using codec: Format[A]): Either[String, A] =
    Json.parse(json).validate[A] match
      case JsSuccess(value, _) => Right(value)
      case JsError(errors)     => Left(errors.mkString(", "))

  def encode[A](value: A)(using codec: Format[A]): String =
    Json.stringify(Json.toJson(value))

// NOTE: The Json.format macro seems to just be broken with any case class...
given Format[Composite] with
  override def writes(o: Composite): JsValue =
    Json.obj(
      "newtype"       -> Json.toJson(o.newtype),
      "simpleNewtype" -> Json.toJson(o.simpleNewtype),
      "subtype"       -> Json.toJson(o.subtype),
      "simpleSubtype" -> Json.toJson(o.simpleSubtype)
    )

  override def reads(json: JsValue): JsResult[Composite] =
    for
      newtype       <- (json \ "newtype").validate[ValidatedNewtype]
      simpleNewtype <- (json \ "simpleNewtype").validate[SimpleNewtype]
      subtype       <- (json \ "subtype").validate[ValidatedSubtype]
      simpleSubtype <- (json \ "simpleSubtype").validate[SimpleSubtype]
    yield Composite(newtype, simpleNewtype, subtype, simpleSubtype)

object PlayJsonSpec extends JsonLibrarySpec[Format]("ZioJson", PlayJsonLibrary)
