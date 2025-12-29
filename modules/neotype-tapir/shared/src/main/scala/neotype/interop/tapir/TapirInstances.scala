package neotype.interop.tapir

import neotype.*
import sttp.tapir.Codec
import sttp.tapir.CodecFormat
import sttp.tapir.DecodeResult
import sttp.tapir.Schema
import sttp.tapir.ValidationResult
import sttp.tapir.Validator
import sttp.tapir.json.pickler.*

given [A, B](using nt: WrappedType[A, B], schema: Schema[A]): Schema[B] =
  schema
    .validate(
      Validator.custom(
        b => //
          nt.make(b) match
            case Left(message) => ValidationResult.Invalid(message)
            case Right(_)      => ValidationResult.Valid
        ,
        None
      )
    )
    .map(nt.make(_).toOption)(nt.unwrap)

given [L, A, B, CF <: CodecFormat](using nt: WrappedType[A, B], codec: Codec[L, A, CF]): Codec[L, B, CF] =
  codec
    .validate(
      Validator.custom(
        b => //
          nt.make(b) match
            case Left(message) => ValidationResult.Invalid(message)
            case Right(_)      => ValidationResult.Valid
        ,
        None
      )
    )
    .mapDecode(a => DecodeResult.fromEitherString(a.toString, nt.make(a)))(nt.unwrap)

given [A, B](using nt: WrappedType[A, B], pickler: Pickler[A]): Pickler[B] =
  Pickler(
    new TapirPickle[B]:
      override lazy val writer: Writer[B] =
        pickler.innerUpickle.writer.asInstanceOf[Writer[B]]

      override lazy val reader: Reader[B] =
        pickler.innerUpickle.reader
          .map { a =>
            nt.make(a) match
              case Left(value)  => throw new RuntimeException(value)
              case Right(value) => value
          }
          .asInstanceOf[Reader[B]]
    ,
    pickler.schema
      .validate(
        Validator.custom(
          b =>
            nt.make(b) match
              case Left(message) => ValidationResult.Invalid(message)
              case Right(_)      => ValidationResult.Valid
          ,
          None
        )
      )
      .map(nt.make(_).toOption)(nt.unwrap)
  )
