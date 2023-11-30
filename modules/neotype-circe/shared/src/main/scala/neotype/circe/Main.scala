package neotype.circe

import neotype.*
import io.circe.*
import io.circe.syntax.*

// Newtype
given [A, B](using newType: Newtype.WithType[A, B], decoder: Decoder[A]): Decoder[B] =
  decoder.emap(newType.make)

given [A, B](using newType: Newtype.WithType[A, B], encoder: Encoder[A]): Encoder[B] =
  encoder.contramap(_.unwrap)

given [A, B](using newType: Newtype.WithType[A, B], codec: Codec[A]): Codec[B] =
  codec.iemap(newType.make)(_.unwrap)

// Newtype.Simple

given [A, B](using newtype: Newtype.Simple.WithType[A, B], decoder: Decoder[A]): Decoder[B] =
  newtype.applyF(decoder)

given [A, B](using newtype: Newtype.Simple.WithType[A, B], encoder: Encoder[A]): Encoder[B] =
  newtype.applyF(encoder)

given [A, B](using newtype: Newtype.Simple.WithType[A, B], codec: Codec[A]): Codec[B] =
  newtype.applyF(codec)

// Subtype

given [A, B <: A](using subtype: Subtype.WithType[A, B], decoder: Decoder[A]): Decoder[B] =
  decoder.emap(subtype.make)

given [A, B <: A](using subtype: Subtype.WithType[A, B], encoder: Encoder[A]): Encoder[B] =
  encoder.contramap(identity)

given [A, B <: A](using subtype: Subtype.WithType[A, B], codec: Codec[A]): Codec[B] =
  codec.iemap(subtype.make)(identity)

// Subtype.Simple

given [A, B <: A](using subtype: Subtype.Simple.WithType[A, B], decoder: Decoder[A]): Decoder[B] =
  subtype.applyF(decoder)

given [A, B <: A](using subtype: Subtype.Simple.WithType[A, B], encoder: Encoder[A]): Encoder[B] =
  subtype.applyF(encoder)

given [A, B <: A](using subtype: Subtype.Simple.WithType[A, B], codec: Codec[A]): Codec[B] =
  subtype.applyF(codec)
