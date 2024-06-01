package neotype.interop.zioschema

import neotype.*
import zio.schema.Schema

// Validated Newtype
given [A, B](using
    newtype: Newtype.WithType[A, B],
    schema: Schema[A],
    isValidatedType: IsValidatedType[newtype.type]
): Schema[B] =
  schema.transformOrFail(a => newtype.make(a), b => Right(b.unwrap))

// Simple Newtype
given [A, B](using
    newtype: Newtype.WithType[A, B],
    schema: Schema[A],
    isSimpleType: IsSimpleType[newtype.type]
): Schema[B] =
  newtype.unsafeMakeF(schema)

// Validated Subtype
given [A, B <: A](using
    subtype: Subtype.WithType[A, B],
    schema: Schema[A],
    isValidatedType: IsValidatedType[subtype.type]
): Schema[B] =
  schema.transformOrFail(a => subtype.make(a), b => Right(b))

// Simple Subtype
given [A, B <: A](using
    subtype: Subtype.WithType[A, B],
    schema: Schema[A],
    isSimpleType: IsSimpleType[subtype.type]
): Schema[B] =
  subtype.unsafeMakeF(schema)
