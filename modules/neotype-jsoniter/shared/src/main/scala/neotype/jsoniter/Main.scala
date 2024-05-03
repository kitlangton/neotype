package neotype.jsoniter

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
): JsonValueCodec[B] =
  new JsonValueCodec[B]:
    private val codec = JsonCodecMaker.make[A](config)

    override def decodeValue(in: JsonReader, default: B): B =
      val decoded = codec.decodeValue(in, newType.unwrap(default))
      newType.make(decoded) match
        case Right(value) => value
        case Left(value)  => in.decodeError(s"Failed to decode $value")

    override def encodeValue(x: B, out: JsonWriter): Unit =
      codec.encodeValue(newType.unwrap(x), out)

    override def nullValue: B = null.asInstanceOf[B]

// Subtype
inline given subtypeCodec[A, B <: A](using
    subType: Subtype.WithType[A, B],
    inline config: CodecMakerConfig = CodecMakerConfig
): JsonValueCodec[B] =
  new JsonValueCodec[B]:
    private val codec = JsonCodecMaker.make[A](config)

    override def decodeValue(in: JsonReader, default: B): B =
      val decoded = codec.decodeValue(in, default)
      subType.make(decoded) match
        case Right(value) => value
        case Left(value)  => in.decodeError(s"Failed to decode $value")

    override def encodeValue(x: B, out: JsonWriter): Unit =
      codec.encodeValue(x, out)

    override def nullValue: B = null.asInstanceOf[B]
