package neotype.interop.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import neotype.*

// Newtype
inline given newtypeCodec[A, B](using
    newType: Newtype.WithType[A, B],
    inline config: CodecMakerConfig = CodecMakerConfig
): JsonValueCodec[B] = new NewtypeJsonValueCodec(JsonCodecMaker.make[A](config), newType)

private class NewtypeJsonValueCodec[A, B](aCodec: JsonValueCodec[A], newType: Newtype.WithType[A, B])
    extends JsonValueCodec[B]:
  override def decodeValue(in: JsonReader, default: B): B =
    newType.make(aCodec.decodeValue(in, newType.unwrap(default))) match
      case Right(value) => value
      case Left(value)  => in.decodeError(s"Failed to decode $value")

  override def encodeValue(x: B, out: JsonWriter): Unit = aCodec.encodeValue(newType.unwrap(x), out)

  override def nullValue: B = null.asInstanceOf[B]

// Subtype
inline given subtypeCodec[A, B <: A](using
    subType: Subtype.WithType[A, B],
    inline config: CodecMakerConfig = CodecMakerConfig
): JsonValueCodec[B] = new SubtypeJsonValueCodec(JsonCodecMaker.make[A](config), subType)

private class SubtypeJsonValueCodec[A, B <: A](aCodec: JsonValueCodec[A], subType: Subtype.WithType[A, B])
    extends JsonValueCodec[B]:
  override def decodeValue(in: JsonReader, default: B): B =
    subType.make(aCodec.decodeValue(in, default)) match
      case Right(value) => value
      case Left(value)  => in.decodeError(s"Failed to decode $value")

  override def encodeValue(x: B, out: JsonWriter): Unit = aCodec.encodeValue(x, out)

  override def nullValue: B = null.asInstanceOf[B]
