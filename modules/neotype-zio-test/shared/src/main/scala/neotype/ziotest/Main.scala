package neotype.ziotest

import neotype.*
import _root_.zio.test.*
import zio.test.magnolia.DeriveGen

// Newtype.Simple
given [A, B](using newType: Newtype.Simple.WithType[A, B], gen: DeriveGen[A]): DeriveGen[B] =
  newType.applyF(gen)

// Subtype.Simple
given [A, B <: A](using subType: Subtype.Simple.WithType[A, B], gen: DeriveGen[A]): DeriveGen[B] =
  subType.applyF(gen)
