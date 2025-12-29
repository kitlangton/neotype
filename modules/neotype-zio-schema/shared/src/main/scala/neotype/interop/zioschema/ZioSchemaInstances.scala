package neotype.interop.zioschema

import neotype.*
import zio.schema.Schema

// Validated Wrapper
given [A, B](using nt: ValidatedWrappedType[A, B], schema: Schema[A]): Schema[B] =
  schema.transformOrFail(a => nt.make(a), b => Right(nt.unwrap(b)))

// Simple Wrapper
given [A, B](using nt: SimpleWrappedType[A, B], schema: Schema[A]): Schema[B] =
  nt.unsafeMakeF(schema)
