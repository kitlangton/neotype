package neotype.interop.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import neotype.*

inline given neotypeCodec[A, B](using
    nt: WrappedType[A, B],
    inline config: CodecMakerConfig = CodecMakerConfig
): JsonValueCodec[B] = new WrappedTypeJsonValueCodec(JsonCodecMaker.make[A](config), nt)

private class WrappedTypeJsonValueCodec[A, B](aCodec: JsonValueCodec[A], nt: WrappedType[A, B])
    extends JsonValueCodec[B]:
  override def decodeValue(in: JsonReader, default: B): B =
    val aDefault =
      if default == null then aCodec.nullValue
      else nt.unwrap(default)
    nt.make(aCodec.decodeValue(in, aDefault)) match
      case Right(value) => value
      case Left(value)  => in.decodeError(s"Failed to decode $value")

  override def encodeValue(x: B, out: JsonWriter): Unit = aCodec.encodeValue(nt.unwrap(x), out)

  override def nullValue: B = null.asInstanceOf[B]
