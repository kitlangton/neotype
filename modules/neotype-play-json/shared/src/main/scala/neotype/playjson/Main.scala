package neotype.playjson

import neotype.*
import play.api.libs.json.*

///////////////////////
// Validated Newtype //
///////////////////////

given newtypeReads[A, B](using
    newtype: Newtype.WithType[A, B],
    reads: Reads[A],
    isValidatedType: IsValidatedType[newtype.type]
): Reads[B] =
  reads.flatMapResult { a =>
    newtype.make(a) match
      case Right(b)  => JsSuccess(b)
      case Left(err) => JsError(err)
  }

given newtypeWrites[A, B](using
    newtype: Newtype.WithType[A, B],
    writes: Writes[A]
): Writes[B] =
  newtype.unsafeMakeF(writes)

given newtypeFormat[A, B](using
    newtype: Newtype.WithType[A, B],
    format: Format[A],
    isValidatedType: IsValidatedType[newtype.type]
): Format[B] =
  new Format[B]:
    def reads(json: JsValue): JsResult[B] =
      json.validate[A].flatMap { a =>
        newtype.make(a) match
          case Right(b)  => JsSuccess(b)
          case Left(err) => JsError(err)
      }

    def writes(b: B): JsValue =
      format.writes(newtype.unwrap(b))

////////////////////
// Simple Newtype //
////////////////////

given simpleNewtypeReads[A, B](using
    newtype: Newtype.WithType[A, B],
    reads: Reads[A],
    isSimpleType: IsSimpleType[newtype.type]
): Reads[B] =
  newtype.unsafeMakeF(reads)

given simpleNewtypeFormat[A, B](using
    newtype: Newtype.WithType[A, B],
    format: Format[A],
    isSimpleType: IsSimpleType[newtype.type]
): Format[B] =
  newtype.unsafeMakeF(format)

///////////////////////
// Validated Subtype //
///////////////////////

given subtypeReads[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    reads: Reads[A],
    isValidatedType: IsValidatedType[subtype.type]
): Reads[B] =
  reads.flatMapResult { a =>
    subtype.make(a) match
      case Right(b)  => JsSuccess(b)
      case Left(err) => JsError(err)
  }

given subtypeWrites[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    writes: Writes[A]
): Writes[B] =
  subtype.unsafeMakeF(writes)

given subtypeFormat[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    format: Format[A],
    isValidatedType: IsValidatedType[subtype.type]
): Format[B] =
  new Format[B]:
    def reads(json: JsValue): JsResult[B] =
      json.validate[A].flatMap { a =>
        subtype.make(a) match
          case Right(b)  => JsSuccess(b)
          case Left(err) => JsError(err)
      }

    def writes(b: B): JsValue =
      format.writes(b)

///////////////////
// Simple Subtype //
///////////////////

given simpleSubtypeReads[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    reads: Reads[A],
    isSimpleType: IsSimpleType[subtype.type]
): Reads[B] =
  subtype.unsafeMakeF(reads)

given simpleSubtypeFormat[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    format: Format[A],
    isSimpleType: IsSimpleType[subtype.type]
): Format[B] =
  subtype.unsafeMakeF(format)
