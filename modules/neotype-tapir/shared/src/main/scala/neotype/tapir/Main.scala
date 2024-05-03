package neotype.tapir

import neotype.*
import sttp.tapir.Codec
import sttp.tapir.CodecFormat
import sttp.tapir.DecodeResult
import sttp.tapir.Schema
import sttp.tapir.ValidationResult
import sttp.tapir.Validator
import sttp.tapir.json.pickler.*

// Newtype
given [A, B](using newtype: Newtype.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema
    .validate(
      Validator.custom(
        b => //
          newtype.make(b) match
            case Left(message) => ValidationResult.Invalid(message)
            case Right(_)      => ValidationResult.Valid
        ,
        None
      )
    )
    .map(newtype.make(_).toOption)(_.unwrap)

given [L, A, B, CF <: CodecFormat](using newtype: Newtype.WithType[A, B], codec: Codec[L, A, CF]): Codec[L, B, CF] =
  codec
    .validate(
      Validator.custom(
        b => //
          newtype.make(b) match
            case Left(message) => ValidationResult.Invalid(message)
            case Right(_)      => ValidationResult.Valid
        ,
        None
      )
    )
    .mapDecode(a => DecodeResult.fromEitherString(a.toString, newtype.make(a)))(_.unwrap)

// Subtype
given [A, B <: A](using subtype: Subtype.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema
    .validate(
      Validator.custom(
        b => //
          subtype.make(b) match
            case Left(message) => ValidationResult.Invalid(message)
            case Right(_)      => ValidationResult.Valid
        ,
        None
      )
    )
    .map(subtype.make(_).toOption)(identity)

given [L, A, B <: A, CF <: CodecFormat](using
    subtype: Subtype.WithType[A, B],
    codec: Codec[L, A, CF]
): Codec[L, B, CF] =
  codec
    .validate(
      Validator.custom(
        b => //
          subtype.make(b) match
            case Left(message) => ValidationResult.Invalid(message)
            case Right(_)      => ValidationResult.Valid
        ,
        None
      )
    )
    .mapDecode(a => DecodeResult.fromEitherString(a.toString, subtype.make(a)))(identity)

// Newtype.WithType
given [A, B](using newtype: Newtype.WithType[A, B], pickler: Pickler[A]): Pickler[B] =
  Pickler(
    new TapirPickle[B]:
      override lazy val writer: Writer[B] =
        pickler.innerUpickle.writer.asInstanceOf[Writer[B]]

      override lazy val reader: Reader[B] =
        pickler.innerUpickle.reader
          .map { a =>
            newtype.make(a) match
              case Left(value)  => throw new RuntimeException(value)
              case Right(value) => value
          }
          .asInstanceOf[Reader[B]]
    ,
    pickler.schema
      .validate(
        Validator.custom(
          b =>
            newtype.make(b) match
              case Left(message) => ValidationResult.Invalid(message)
              case Right(_)      => ValidationResult.Valid
          ,
          None
        )
      )
      .map(newtype.make(_).toOption)(_.unwrap)
  )

// Subtype.WithType
given [A, B <: A](using subtype: Subtype.WithType[A, B], pickler: Pickler[A]): Pickler[B] =
  Pickler(
    new TapirPickle[B]:
      override lazy val writer: Writer[B] =
        pickler.innerUpickle.writer.asInstanceOf[Writer[B]]

      override lazy val reader: Reader[B] =
        pickler.innerUpickle.reader
          .map { a =>
            subtype.make(a) match
              case Left(value)  => throw new RuntimeException(value)
              case Right(value) => value
          }
          .asInstanceOf[Reader[B]]
    ,
    pickler.schema
      .validate {
        Validator.custom(
          b =>
            subtype.make(b) match
              case Left(message) => ValidationResult.Invalid(message)
              case Right(_)      => ValidationResult.Valid
          ,
          None
        )
      }
      .map(subtype.make(_).toOption)(identity)
  )
