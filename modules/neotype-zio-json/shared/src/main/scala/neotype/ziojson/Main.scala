package neotype.ziojson

import neotype.*
import zio.json.*

///////////////////////
// Validated Newtype //
///////////////////////

given [A, B](using
    newtype: Newtype.WithType[A, B],
    decoder: JsonDecoder[A],
    isValidatedType: IsValidatedType[newtype.type]
): JsonDecoder[B] =
  decoder.mapOrFail(newtype.make(_))

given [A, B](using
    newtype: Newtype.WithType[A, B],
    encoder: JsonEncoder[A]
): JsonEncoder[B] =
  newtype.unsafeMakeF(encoder)

given [A, B](using
    newtype: Newtype.WithType[A, B],
    codec: JsonCodec[A],
    isValidatedType: IsValidatedType[newtype.type]
): JsonCodec[B] =
  codec.transformOrFail(newtype.make(_), newtype.unwrap(_))

////////////////////
// Simple Newtype //
////////////////////

given [A, B](using
    newtype: Newtype.WithType[A, B],
    decoder: JsonDecoder[A],
    isSimpleType: IsSimpleType[newtype.type]
): JsonDecoder[B] =
  newtype.unsafeMakeF(decoder)

given [A, B](using
    newtype: Newtype.WithType[A, B],
    codec: JsonCodec[A],
    isSimpleType: IsSimpleType[newtype.type]
): JsonCodec[B] =
  newtype.unsafeMakeF(codec)

///////////////////////
// Validated Subtype //
///////////////////////

inline given [A, B <: A](using
    subtype: Subtype.WithType[A, B],
    decoder: JsonDecoder[A],
    isValidatedType: IsValidatedType[subtype.type]
): JsonDecoder[B] =
  decoder.mapOrFail(a => subtype.make(a))

given [A, B <: A](using
    subtype: Subtype.WithType[A, B],
    encoder: JsonEncoder[A]
): JsonEncoder[B] =
  subtype.unsafeMakeF(encoder)

given [A, B <: A](using
    subtype: Subtype.WithType[A, B],
    codec: JsonCodec[A],
    isValidatedType: IsValidatedType[subtype.type]
): JsonCodec[B] =
  codec.transformOrFail(subtype.make(_), identity)

////////////////////
// Simple Subtype //
////////////////////

inline given [A, B <: A](using
    subtype: Subtype.WithType[A, B],
    decoder: JsonDecoder[A],
    isSimpleType: IsSimpleType[subtype.type]
): JsonDecoder[B] =
  subtype.unsafeMakeF(decoder)

given [A, B <: A](using
    subtype: Subtype.WithType[A, B],
    codec: JsonCodec[A],
    isSimpleType: IsSimpleType[subtype.type]
): JsonCodec[B] =
  subtype.unsafeMakeF(codec)
