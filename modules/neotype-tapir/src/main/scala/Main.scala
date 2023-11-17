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
given newTypeWithTypeSchema[A, B](using newType: Newtype.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema
    .validate(
      Validator.custom(b => ValidationResult.validWhen(newType.validate(b)), Some(newType.failureMessage))
    )
    .map(newType.make(_).toOption)(_.unwrap)

given newTypeWithTypeCodec[L, A, B, CF <: CodecFormat](using
    newType: Newtype.WithType[A, B],
    codec: Codec[L, A, CF]
): Codec[L, B, CF] =
  codec
    .validate(Validator.custom(b => ValidationResult.validWhen(newType.validate(b)), Some(newType.failureMessage)))
    .mapDecode(a => DecodeResult.fromEitherString(a.toString, newType.make(a)))(_.unwrap)

// Newtype.Simple
given newTypeSimpleSchema[A, B](using newType: Newtype.Simple.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema.map(a => Some(newType(a)))(_.unwrap)

given newTypeSimpleCodec[L, A, B, CF <: CodecFormat](using
    newType: Newtype.Simple.WithType[A, B],
    codec: Codec[L, A, CF]
): Codec[L, B, CF] =
  codec.map(newType(_))(_.unwrap)

// Subtype
given subtypeWithTypeSchema[A, B <: A](using subType: Subtype.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema
    .validate(
      Validator.custom(b => ValidationResult.validWhen(subType.validate(b)), Some(subType.failureMessage))
    )
    .map(subType.make(_).toOption)(identity)

given subtypeWithTypeCodec[L, A, B <: A, CF <: CodecFormat](using
    subType: Subtype.WithType[A, B],
    codec: Codec[L, A, CF]
): Codec[L, B, CF] =
  codec
    .validate(Validator.custom(b => ValidationResult.validWhen(subType.validate(b)), Some(subType.failureMessage)))
    .mapDecode(a => DecodeResult.fromEitherString(a.toString, subType.make(a)))(identity)

// Subtype.Simple
given subtypeSimpleSchema[A, B <: A](using subType: Subtype.Simple.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema.map(a => Some(subType(a)))(identity)

given subtypeSimpleCodec[L, A, B <: A, CF <: CodecFormat](using
    subType: Subtype.Simple.WithType[A, B],
    codec: Codec[L, A, CF]
): Codec[L, B, CF] =
  codec.map(subType(_))(identity)

// Newtype.WithType
given newTypeWithTypePickler[A, B](using newType: Newtype.WithType[A, B], pickler: Pickler[A]): Pickler[B] =
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

// Newtype.Simple.WithType
given newTypeSimplePickler[A, B](using newType: Newtype.Simple.WithType[A, B], pickler: Pickler[A]): Pickler[B] =
  Pickler(
    new TapirPickle[B]:
      override lazy val writer: Writer[B] =
        pickler.innerUpickle.writer.asInstanceOf[Writer[B]]

      override lazy val reader: Reader[B] =
        pickler.innerUpickle.reader.asInstanceOf[Reader[B]]
    ,
    pickler.schema.map(a => Some(newType(a)))(_.unwrap)
  )

// Subtype.WithType
given subtypeWithTypePickler[A, B <: A](using subType: Subtype.WithType[A, B], pickler: Pickler[A]): Pickler[B] =
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

// Subtype.Simple.WithType
given subTypeSimplePickler[A, B <: A](using subType: Subtype.Simple.WithType[A, B], pickler: Pickler[A]): Pickler[B] =
  Pickler(
    new TapirPickle[B]:
      override lazy val writer: Writer[B] =
        pickler.innerUpickle.writer.asInstanceOf[Writer[B]]

      override lazy val reader: Reader[B] =
        pickler.innerUpickle.reader.asInstanceOf[Reader[B]]
    ,
    pickler.schema.map(a => Some(subType(a)))(identity)
  )
