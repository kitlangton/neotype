package neotype.interop.circe

import io.circe.*
import neotype.*

// Newtype
given [A, B](using newType: Newtype.WithType[A, B], decoder: Decoder[A]): Decoder[B] =
  decoder.emap(newType.make(_))

given [A, B](using newType: Newtype.WithType[A, B], encoder: Encoder[A]): Encoder[B] =
  encoder.contramap(_.unwrap)

given [A, B](using newType: Newtype.WithType[A, B], codec: Codec[A]): Codec[B] =
  codec.iemap(newType.make(_))(_.unwrap)

// Subtype

given [A, B <: A](using subtype: Subtype.WithType[A, B], decoder: Decoder[A]): Decoder[B] =
  decoder.emap(subtype.make(_))

given [A, B <: A](using subtype: Subtype.WithType[A, B], encoder: Encoder[A]): Encoder[B] =
  encoder.contramap(identity)

given [A, B <: A](using subtype: Subtype.WithType[A, B], codec: Codec[A]): Codec[B] =
  codec.iemap(subtype.make(_))(identity)
