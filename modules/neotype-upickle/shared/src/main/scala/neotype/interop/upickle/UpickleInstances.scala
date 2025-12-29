package neotype.interop.upickle

import neotype.*
import upickle.default.*

// Validated Wrapper
given validatedReader[A, B](using nt: ValidatedWrappedType[A, B], rwA: Reader[A]): Reader[B] =
  rwA.map[B](a => nt.make(a).fold(err => throw new upickle.core.Abort(err), identity))

given writer[A, B](using nt: WrappedType[A, B], rwA: Writer[A]): Writer[B] =
  nt.unsafeMakeF(rwA)

given validatedReadWriter[A, B](using nt: ValidatedWrappedType[A, B], rwA: ReadWriter[A]): ReadWriter[B] =
  rwA.bimap[B](
    b => nt.unwrap(b),
    a => nt.make(a).fold(err => throw new upickle.core.Abort(err), identity)
  )

// Simple Wrapper
given simpleReader[A, B](using nt: SimpleWrappedType[A, B], rwA: Reader[A]): Reader[B] =
  nt.unsafeMakeF(rwA)

given simpleReadWriter[A, B](using nt: SimpleWrappedType[A, B], rwA: ReadWriter[A]): ReadWriter[B] =
  rwA.bimap[B](
    b => nt.unwrap(b),
    a => nt.unsafeMake(a)
  )
