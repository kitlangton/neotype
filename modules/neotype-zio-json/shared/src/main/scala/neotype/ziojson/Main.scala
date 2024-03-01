package neotype.ziojson

import neotype.*
import zio.json.*

// Newtype
given [A, B](using newType: Newtype.WithType[A, B], decoder: JsonDecoder[A]): JsonDecoder[B] =
  decoder.mapOrFail(newType.make(_))

given [A, B](using newType: Newtype.WithType[A, B], encoder: JsonEncoder[A]): JsonEncoder[B] =
  encoder.contramap(_.unwrap)

given [A, B](using newType: Newtype.WithType[A, B], codec: JsonCodec[A]): JsonCodec[B] =
  codec.transformOrFail(newType.make(_), _.unwrap)

// Newtype.Simple

given [A, B](using newtype: Newtype.Simple.WithType[A, B], decoder: JsonDecoder[A]): JsonDecoder[B] =
  newtype.applyF(decoder)

given [A, B](using newtype: Newtype.Simple.WithType[A, B], encoder: JsonEncoder[A]): JsonEncoder[B] =
  newtype.applyF(encoder)

given [A, B](using newtype: Newtype.Simple.WithType[A, B], codec: JsonCodec[A]): JsonCodec[B] =
  newtype.applyF(codec)

// Subtype

inline given [A, B <: A](using subtype: Subtype.WithType[A, B], decoder: JsonDecoder[A]): JsonDecoder[B] =
  decoder.mapOrFail(a => subtype.make(a))

given [A, B <: A](using subtype: Subtype.WithType[A, B], encoder: JsonEncoder[A]): JsonEncoder[B] =
  encoder.contramap(identity)

given [A, B <: A](using subtype: Subtype.WithType[A, B], codec: JsonCodec[A]): JsonCodec[B] =
  codec.transformOrFail(a => subtype.make(a), identity)

// Subtype.Simple

given [A, B <: A](using subtype: Subtype.Simple.WithType[A, B], decoder: JsonDecoder[A]): JsonDecoder[B] =
  subtype.applyF(decoder)

given [A, B <: A](using subtype: Subtype.Simple.WithType[A, B], encoder: JsonEncoder[A]): JsonEncoder[B] =
  subtype.applyF(encoder)

given [A, B <: A](using subtype: Subtype.Simple.WithType[A, B], codec: JsonCodec[A]): JsonCodec[B] =
  subtype.applyF(codec)
