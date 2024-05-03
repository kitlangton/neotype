package neotype.ziotest

import _root_.zio.test.*
import neotype.*
import zio.test.magnolia.DeriveGen

// Newtype.Simple
given [A, B](using
    newType: Newtype.WithType[A, B],
    gen: DeriveGen[A],
    isSimple: IsSimpleType[newType.type]
): DeriveGen[B] =
  newType.unsafeMakeF(gen)

// Subtype.Simple
given [A, B <: A](using
    subType: Subtype.WithType[A, B],
    gen: DeriveGen[A],
    isSimple: IsSimpleType[subType.type]
): DeriveGen[B] =
  subType.unsafeMakeF(gen)
