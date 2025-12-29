package neotype.interop.playjson

import neotype.*
import play.api.libs.json.*

//////////////////////
// Validated Wrapper //
//////////////////////

given reads[A, B](using nt: ValidatedWrappedType[A, B], reads: Reads[A]): Reads[B] =
  reads.flatMapResult { a =>
    nt.make(a) match
      case Right(b)  => JsSuccess(b)
      case Left(err) => JsError(err)
  }

given writes[A, B](using nt: WrappedType[A, B], writes: Writes[A]): Writes[B] =
  nt.unsafeMakeF(writes)

given format[A, B](using nt: ValidatedWrappedType[A, B], format: Format[A]): Format[B] =
  new Format[B]:
    def reads(json: JsValue): JsResult[B] =
      json.validate[A].flatMap { a =>
        nt.make(a) match
          case Right(b)  => JsSuccess(b)
          case Left(err) => JsError(err)
      }

    def writes(b: B): JsValue =
      format.writes(nt.unwrap(b))

///////////////////
// Simple Wrapper //
///////////////////

given simpleReads[A, B](using nt: SimpleWrappedType[A, B], reads: Reads[A]): Reads[B] =
  nt.unsafeMakeF(reads)

given simpleFormat[A, B](using nt: SimpleWrappedType[A, B], format: Format[A]): Format[B] =
  nt.unsafeMakeF(format)
