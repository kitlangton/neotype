package neotype.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import neotype.*

// Newtype
inline given [A, B](using newType: Newtype.WithType[A, B]): JsonValueCodec[B] = new JsonValueCodec[B]:
  private val codec = JsonCodecMaker.make[A]

  override def decodeValue(in: JsonReader, default: B): B =
    val decoded = codec.decodeValue(in, newType.unwrap(default))
    newType.make(decoded) match
      case Left(value)  => in.decodeError(s"Failed to decode $value")
      case Right(value) => value

  override def encodeValue(x: B, out: JsonWriter): Unit =
    codec.encodeValue(newType.unwrap(x), out)

  override def nullValue: B = null.asInstanceOf[B]

// Newtype.Simple
inline given [A, B](using newType: Newtype.Simple.WithType[A, B]): JsonValueCodec[B] =
  newType.applyF(JsonCodecMaker.make[A])

// Subtype
inline given [A, B <: A](using subType: Subtype.WithType[A, B]): JsonValueCodec[B] =
  new JsonValueCodec[B]:
    private val codec = JsonCodecMaker.make[A]

    override def decodeValue(in: JsonReader, default: B): B =
      val decoded = codec.decodeValue(in, default)
      subType.make(decoded) match
        case Left(value)  => in.decodeError(s"Failed to decode $value")
        case Right(value) => value

    override def encodeValue(x: B, out: JsonWriter): Unit =
      codec.encodeValue(x, out)

    override def nullValue: B = null.asInstanceOf[B]

// Subtype.Simple
inline given [A, B <: A](using subType: Subtype.Simple.WithType[A, B]): JsonValueCodec[B] =
  subType.applyF(JsonCodecMaker.make[A])
