package neotype.interop.chimney

import io.scalaland.chimney.*
import io.scalaland.chimney.partial.syntax.*
import neotype.*

// Transform a WrappedType to its inner type
given [A, B](using nt: WrappedType[A, B]): Transformer[B, A] =
  nt.unwrap(_)

// Transform a regular type to a simple WrappedType
given [A, B](using nt: SimpleWrappedType[A, B]): Transformer[A, B] =
  nt.unsafeMake(_)

// Transform a regular type to a validated WrappedType, may fail so we use PartialTransformer
given [A, B](using nt: ValidatedWrappedType[A, B]): PartialTransformer[A, B] =
  PartialTransformer(nt.make(_).asResult)

// Transform from WrappedType
given [A, B, C](using nt: WrappedType[A, B], underlyingTransformer: Transformer[A, C]): Transformer[B, C] =
  (b: B) => underlyingTransformer.transform(nt.unwrap(b))

// Partially transform from WrappedType
given [A, B, C](using
    nt: WrappedType[A, B],
    underlyingPartialTransformer: PartialTransformer[A, C]
): PartialTransformer[B, C] =
  (b: B, failFast: Boolean) => underlyingPartialTransformer.transform(nt.unwrap(b), failFast)
