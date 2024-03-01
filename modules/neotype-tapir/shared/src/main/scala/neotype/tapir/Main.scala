package neotype.tapir

import neotype.*
import sttp.tapir.Schema
import sttp.tapir.Validator
import sttp.tapir.ValidationResult
import sttp.tapir.Codec
import sttp.tapir.DecodeResult
import sttp.tapir.CodecFormat
import sttp.tapir.json.pickler.*

// Newtype
given [A, B](using newType: Newtype.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema
    .validate(
      Validator.custom(b => ValidationResult.validWhen(newType.validate(b)), Some(newType.failureMessage))
    )
    .map(newType.make(_).toOption)(_.unwrap)

given [L, A, B, CF <: CodecFormat](using newType: Newtype.WithType[A, B], codec: Codec[L, A, CF]): Codec[L, B, CF] =
  codec
    .validate(Validator.custom(b => ValidationResult.validWhen(newType.validate(b)), Some(newType.failureMessage)))
    .mapDecode(a => DecodeResult.fromEitherString(a.toString, newType.make(a)))(_.unwrap)

// Subtype
given [A, B <: A](using subType: Subtype.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema
    .validate(
      Validator.custom(b => ValidationResult.validWhen(subType.validate(b)), Some(subType.failureMessage))
    )
    .map(subType.make(_).toOption)(identity)

given [L, A, B <: A, CF <: CodecFormat](using
    subType: Subtype.WithType[A, B],
    codec: Codec[L, A, CF]
): Codec[L, B, CF] =
  codec
    .validate(Validator.custom(b => ValidationResult.validWhen(subType.validate(b)), Some(subType.failureMessage)))
    .mapDecode(a => DecodeResult.fromEitherString(a.toString, subType.make(a)))(identity)

// Newtype.WithType
given [A, B](using newType: Newtype.WithType[A, B], pickler: Pickler[A]): Pickler[B] =
  Pickler(
    new TapirPickle[B]:
      override lazy val writer: Writer[B] =
        pickler.innerUpickle.writer.asInstanceOf[Writer[B]]

      override lazy val reader: Reader[B] =
        pickler.innerUpickle.reader
          .map { a =>
            newType.make(a) match
              case Left(value)  => throw new RuntimeException(value)
              case Right(value) => value
          }
          .asInstanceOf[Reader[B]]
    ,
    pickler.schema
      .validate(Validator.custom(b => ValidationResult.validWhen(newType.validate(b)), Some(newType.failureMessage)))
      .map(newType.make(_).toOption)(_.unwrap)
  )

// Subtype.WithType
given [A, B <: A](using subType: Subtype.WithType[A, B], pickler: Pickler[A]): Pickler[B] =
  Pickler(
    new TapirPickle[B]:
      override lazy val writer: Writer[B] =
        pickler.innerUpickle.writer.asInstanceOf[Writer[B]]

      override lazy val reader: Reader[B] =
        pickler.innerUpickle.reader
          .map { a =>
            subType.make(a) match
              case Left(value)  => throw new RuntimeException(value)
              case Right(value) => value
          }
          .asInstanceOf[Reader[B]]
    ,
    pickler.schema
      .validate(Validator.custom(b => ValidationResult.validWhen(subType.validate(b)), Some(subType.failureMessage)))
      .map(subType.make(_).toOption)(identity)
  )
