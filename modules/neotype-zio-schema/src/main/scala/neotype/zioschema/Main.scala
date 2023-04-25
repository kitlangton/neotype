package neotype.zioschema

import neotype.*
import zio.schema.Schema

// Newtype
given [A, B](using newType: Newtype.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema.transformOrFail(a => newType.make(a), b => Right(b.unwrap))

// Newtype.Simple
given [A, B](using newType: Newtype.Simple.WithType[A, B], schema: Schema[A]): Schema[B] =
  newType.applyF(schema)

// Subtype
given [A, B <: A](using subtype: Subtype.WithType[A, B], schema: Schema[A]): Schema[B] =
  schema.transformOrFail(a => subtype.make(a), b => Right(b))

// Subtype.Simple
given [A, B <: A](using subtype: Subtype.Simple.WithType[A, B], schema: Schema[A]): Schema[B] =
  subtype.applyF(schema)
