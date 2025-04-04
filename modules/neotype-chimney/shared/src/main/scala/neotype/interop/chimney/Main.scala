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

// Transform from Newtype
given [A, B, C](using
    newTypeSource: Newtype.WithType[A, B],
    underlyingTransformer: Transformer[A, C]
): Transformer[B, C] = (b: B) => underlyingTransformer.transform(newTypeSource.unwrap(b))

// Partially transform from Newtype
given [A, B, C](using
    newTypeSource: Newtype.WithType[A, B],
    underlyingPartialTransformer: PartialTransformer[A, C]
): PartialTransformer[B, C] = (b: B, failFast: Boolean) =>
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

// Transform from Subtype
given [A, B <: A, C](using
    subTypeSource: Subtype.WithType[A, B],
    underlyingTransformer: Transformer[A, C]
): Transformer[B, C] = (b: B) => underlyingTransformer.transform(b)

// Partially transform from Subtype
given [A, B <: A, C](using
    subTypeSource: Subtype.WithType[A, B],
    underlyingPartialTransformer: PartialTransformer[A, C]
): PartialTransformer[B, C] = (b: B, failFast: Boolean) => underlyingPartialTransformer.transform(b, failFast)
