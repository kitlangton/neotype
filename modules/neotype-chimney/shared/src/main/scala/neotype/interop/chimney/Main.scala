package neotype.interop.chimney

import io.scalaland.chimney.*
import io.scalaland.chimney.partial.syntax.*
import neotype.*

// Newtype

// Transform a Newtype to its inner type
given [A, B](using newType: Newtype.WithType[A, B]): Transformer[B, A] =
  newType.unwrap(_)

// Transform a regular type to a simple Newtype
given [A, B](using newType: Newtype.WithType[A, B], isSimpleType: IsSimpleType[newType.type]): Transformer[A, B] =
  newType(_)

// Transform a regular type to a validated Newtype, may fail so we use PartialTransformer
given [A, B](using
    newType: Newtype.WithType[A, B],
    isValidatedType: IsValidatedType[newType.type]
): PartialTransformer[A, B] =
  PartialTransformer(newType.make(_).asResult)

// Transform Newtype to Newtype
given [A, B, C, D](using
    newTypeSource: Newtype.WithType[A, B],
    newTypeDestination: Newtype.WithType[C, D],
    underlyingTransformer: Transformer[A, D]
): Transformer[B, D] = (b: B) => underlyingTransformer.transform(newTypeSource.unwrap(b))

// Transform Newtype to Subtype
given [A, B, C, D <: C](using
    newTypeSource: Newtype.WithType[A, B],
    subTypeDestination: neotype.Subtype.WithType[C, D],
    underlyingTransformer: Transformer[A, D]
): Transformer[B, D] = (b: B) => underlyingTransformer.transform(newTypeSource.unwrap(b))

// Partially transform Newtype to Newtype
given [A, B, C, D](using
    newTypeSource: Newtype.WithType[A, B],
    newTypeDestination: Newtype.WithType[C, D],
    underlyingPartialTransformer: PartialTransformer[A, D]
): PartialTransformer[B, D] = (b: B, failFast: Boolean) =>
  underlyingPartialTransformer.transform(newTypeSource.unwrap(b), failFast)

// Partially transform Newtype to Subtype
given [A, B, C, D <: C](using
    newTypeSource: Newtype.WithType[A, B],
    subTypeDestination: Subtype.WithType[C, D],
    underlyingPartialTransformer: PartialTransformer[A, D]
): PartialTransformer[B, D] = (b: B, failFast: Boolean) =>
  underlyingPartialTransformer.transform(newTypeSource.unwrap(b), failFast)

// Subtype

// Transform a regular type to a simple Subtype
given [A, B <: A](using
    subType: Subtype.WithType[A, B],
    isSimpleType: IsSimpleType[subType.type]
): Transformer[A, B] =
  subType(_)

// Transform a regular type to a Subtype with validation, may fail so we use PartialTransformer
given [A, B <: A](using
    subtype: Subtype.WithType[A, B],
    isValidatedType: IsValidatedType[subtype.type]
): PartialTransformer[A, B] =
  PartialTransformer(subtype.make(_).asResult)

// Transform Subtype to Subtype
given [A, B <: A, C, D <: C](using
    subTypeSource: Subtype.WithType[A, B],
    subTypeDestination: Subtype.WithType[C, D],
    underlyingTransformer: Transformer[A, D]
): Transformer[B, D] = (b: B) => underlyingTransformer.transform(b)

// Transform Subtype to Newtype
given [A, B <: A, C, D](using
    subTypeSource: Subtype.WithType[A, B],
    newTypeDestination: Newtype.WithType[C, D],
    underlyingTransformer: Transformer[A, D]
): Transformer[B, D] = (b: B) => underlyingTransformer.transform(b)

// Partially transform Subtype to Subtype
given [A, B <: A, C, D <: C](using
    subTypeSource: Subtype.WithType[A, B],
    subTypeDestination: Subtype.WithType[C, D],
    underlyingPartialTransformer: PartialTransformer[A, D]
): PartialTransformer[B, D] = (b: B, failFast: Boolean) => underlyingPartialTransformer.transform(b, failFast)

// Partially transform Subtype to Newtype
given [A, B <: A, C, D](using
    subTypeSource: Subtype.WithType[A, B],
    subTypeDestination: Newtype.WithType[C, D],
    underlyingPartialTransformer: PartialTransformer[A, D]
): PartialTransformer[B, D] = (b: B, failFast: Boolean) => underlyingPartialTransformer.transform(b, failFast)
