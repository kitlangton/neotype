package neotype.interop.chimney

import io.scalaland.chimney.*
import io.scalaland.chimney.partial.syntax.*
import neotype.*

// Newtype

// Transform a Newtype to its inner type
given [A, B](using newType: Newtype.WithType[A, B]): Transformer[B, A] =
  newType.unwrap(_)

// Transform a regular type to a Newtype, may fail so we use PartialTransformer
given [A, B](using newType: Newtype.WithType[A, B]): PartialTransformer[A, B] =
  PartialTransformer(newType.make(_).asResult)

// Subtype

// Transform a regular type to a Subtype, may fail so we use PartialTransformer
given [A, B <: A](using subtype: Subtype.WithType[A, B]): PartialTransformer[A, B] =
  PartialTransformer(subtype.make(_).asResult)
