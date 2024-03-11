package neotype.playjson

import neotype.*
import play.api.libs.json.*

///////////////////////
// Validated Newtype //
///////////////////////

implicit def newtypeReads[A, B](implicit
    newtype: Newtype.WithType[A, B],
    reads: Reads[A],
    isValidatedType: IsValidatedType[newtype.type]
): Reads[B] =
  reads.flatMapResult { a =>
    newtype.make(a) match
      case Right(b)  => JsSuccess(b)
      case Left(err) => JsError(err)
  }

implicit def newtypeWrites[A, B](implicit
    newtype: Newtype.WithType[A, B],
    writes: Writes[A]
): Writes[B] =
  newtype.unsafeMakeF(writes)

implicit def newtypeFormat[A, B](implicit
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

implicit def simpleNewtypeReads[A, B](implicit
    newtype: Newtype.WithType[A, B],
    reads: Reads[A],
    isSimpleType: IsSimpleType[newtype.type]
): Reads[B] =
  newtype.unsafeMakeF(reads)

implicit def simpleNewtypeFormat[A, B](implicit
    newtype: Newtype.WithType[A, B],
    format: Format[A],
    isSimpleType: IsSimpleType[newtype.type]
): Format[B] =
  newtype.unsafeMakeF(format)

///////////////////////
// Validated Subtype //
///////////////////////

implicit def subtypeReads[A, B <: A](implicit
    subtype: Subtype.WithType[A, B],
    reads: Reads[A],
    isValidatedType: IsValidatedType[subtype.type]
): Reads[B] =
  reads.flatMapResult { a =>
    subtype.make(a) match
      case Right(b)  => JsSuccess(b)
      case Left(err) => JsError(err)
  }

implicit def subtypeWrites[A, B <: A](implicit
    subtype: Subtype.WithType[A, B],
    writes: Writes[A]
): Writes[B] =
  subtype.unsafeMakeF(writes)

implicit def subtypeFormat[A, B <: A](implicit
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

implicit def simpleSubtypeReads[A, B <: A](implicit
    subtype: Subtype.WithType[A, B],
    reads: Reads[A],
    isSimpleType: IsSimpleType[subtype.type]
): Reads[B] =
  subtype.unsafeMakeF(reads)

implicit def simpleSubtypeFormat[A, B <: A](implicit
    subtype: Subtype.WithType[A, B],
    format: Format[A],
    isSimpleType: IsSimpleType[subtype.type]
): Format[B] =
  subtype.unsafeMakeF(format)
