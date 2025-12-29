package neotype.interop.circe

import io.circe.*
import neotype.*

given [A, B](using nt: WrappedType[A, B], decoder: Decoder[A]): Decoder[B] =
  decoder.emap(nt.make(_))

given [A, B](using nt: WrappedType[A, B], encoder: Encoder[A]): Encoder[B] =
  encoder.contramap(nt.unwrap)

given [A, B](using nt: WrappedType[A, B], codec: Codec[A]): Codec[B] =
  codec.iemap(nt.make(_))(nt.unwrap)
