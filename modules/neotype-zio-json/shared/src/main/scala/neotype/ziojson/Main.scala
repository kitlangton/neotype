package neotype.ziojson

import neotype.*
import zio.json.*

// Newtype
given [A, B](using newtype: Newtype.WithType[A, B], decoder: JsonDecoder[A]): JsonDecoder[B] =
  decoder.mapOrFail(newtype.make(_))

given [A, B](using newtype: Newtype.WithType[A, B], encoder: JsonEncoder[A]): JsonEncoder[B] =
  encoder.contramap(newtype.unwrap(_))

given [A, B](using newtype: Newtype.WithType[A, B], codec: JsonCodec[A]): JsonCodec[B] =
  codec.transformOrFail(newtype.make(_), newtype.unwrap(_))

// Subtype

inline given [A, B <: A](using subtype: Subtype.WithType[A, B], decoder: JsonDecoder[A]): JsonDecoder[B] =
  decoder.mapOrFail(a => subtype.make(a))

given [A, B <: A](using subtype: Subtype.WithType[A, B], encoder: JsonEncoder[A]): JsonEncoder[B] =
  encoder.contramap(identity)

given [A, B <: A](using subtype: Subtype.WithType[A, B], codec: JsonCodec[A]): JsonCodec[B] =
  codec.transformOrFail(a => subtype.make(a), identity)
