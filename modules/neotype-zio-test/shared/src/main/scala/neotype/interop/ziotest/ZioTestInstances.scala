package neotype.interop.ziotest

import neotype.*
import zio.test.magnolia.DeriveGen

given [A, B](using nt: SimpleWrappedType[A, B], gen: DeriveGen[A]): DeriveGen[B] =
  nt.unsafeMakeF(gen)
