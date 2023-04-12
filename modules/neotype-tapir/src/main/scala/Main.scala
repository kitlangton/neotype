package neotype.tapir

import neotype.*
import sttp.tapir.Schema
import sttp.tapir.Validator
import sttp.tapir.ValidationResult
import sttp.tapir.Codec
import sttp.tapir.DecodeResult
import sttp.tapir.CodecFormat

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
    

// Newtype.Simple
given [A, B](using newType: Newtype.Simple.WithType[A, B], schema: Schema[A]): Schema[B] =
      schema.map(a => Some(newType(a)))(_.unwrap)

given [L, A, B, CF <: CodecFormat](using newType: Newtype.Simple.WithType[A, B], codec: Codec[L, A, CF]): Codec[L, B, CF] = 
    codec.map(newType(_))(_.unwrap)

// Subtype
given [A, B <: A](using subType: Subtype.WithType[A, B], schema: Schema[A]): Schema[B] =
    schema
    .validate(
        Validator.custom(b => ValidationResult.validWhen(subType.validate(b)), Some(subType.failureMessage))
    )
    .map(subType.make(_).toOption)(identity)

given [L, A, B <: A, CF <: CodecFormat](using subType: Subtype.WithType[A, B], codec: Codec[L, A, CF]): Codec[L, B, CF] = 
    codec
    .validate(Validator.custom(b => ValidationResult.validWhen(subType.validate(b)), Some(subType.failureMessage)))
    .mapDecode(a => DecodeResult.fromEitherString(a.toString, subType.make(a)))(identity)

// Subtype.Simple
given [A, B <: A](using subType: Subtype.Simple.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema.map(a => Some(subType(a)))(identity)

given [L, A, B <: A, CF <: CodecFormat](using subType: Subtype.Simple.WithType[A, B], codec: Codec[L, A, CF]): Codec[L, B, CF] = 
    codec.map(subType(_))(identity)