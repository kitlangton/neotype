package neotype.interop.ziojson

import neotype.*
import zio.json.*

import scala.util.NotGiven

/////////////////////
// Validated Wrapper //
/////////////////////

given decoder[A, B](using nt: ValidatedWrappedType[A, B], decoder: JsonDecoder[A]): JsonDecoder[B] =
  decoder.mapOrFail(nt.make(_))

given encoder[A, B](using
    nt: WrappedType[A, B],
    encoder: JsonEncoder[A],
    notGivenEncoder: NotGiven[JsonEncoder[B]]
): JsonEncoder[B] =
  nt.unsafeMakeF(encoder)

given codec[A, B](using nt: ValidatedWrappedType[A, B], codec: JsonCodec[A]): JsonCodec[B] =
  codec.transformOrFail(nt.make(_), nt.unwrap(_))

///////////////////
// Simple Wrapper //
///////////////////

given simpleDecoder[A, B](using nt: SimpleWrappedType[A, B], decoder: JsonDecoder[A]): JsonDecoder[B] =
  nt.unsafeMakeF(decoder)

given simpleCodec[A, B](using nt: SimpleWrappedType[A, B], codec: JsonCodec[A]): JsonCodec[B] =
  nt.unsafeMakeF(codec)
