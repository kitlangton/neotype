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

// Subtype

inline given [A, B <: A](using subtype: Subtype.WithType[A, B], decoder: JsonDecoder[A]): JsonDecoder[B] =
  decoder.mapOrFail(a => subtype.make(a))

given [A, B <: A](using subtype: Subtype.WithType[A, B], encoder: JsonEncoder[A]): JsonEncoder[B] =
  encoder.contramap(identity)

given [A, B <: A](using subtype: Subtype.WithType[A, B], codec: JsonCodec[A]): JsonCodec[B] =
  codec.transformOrFail(a => subtype.make(a), identity)
