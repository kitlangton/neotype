package neotype.interop.upickle

import neotype.*
import upickle.default.*

// Validated Newtype
given validatedNewtypeReader[A, B](using
    newtype: Newtype.WithType[A, B],
    rwA: Reader[A],
    isValidatedType: IsValidatedType[newtype.type]
): Reader[B] =
  rwA.map[B](a => newtype.make(a).fold(err => throw new upickle.core.Abort(err), identity))

given validatedNewtypeWriter[A, B](using
    newtype: Newtype.WithType[A, B],
    rwA: Writer[A]
): Writer[B] =
  newtype.unsafeMakeF(rwA)

given validatedNewtypeReadWriter[A, B](using
    newtype: Newtype.WithType[A, B],
    rwA: ReadWriter[A],
    isValidatedType: IsValidatedType[newtype.type]
): ReadWriter[B] =
  rwA.bimap[B](
    b => newtype.unwrap(b),
    a => newtype.make(a).fold(err => throw new upickle.core.Abort(err), identity)
  )

// Simple Newtype
given simpleNewtypeReader[A, B](using
    newtype: Newtype.WithType[A, B],
    rwA: Reader[A],
    isSimpleType: IsSimpleType[newtype.type]
): Reader[B] =
  newtype.unsafeMakeF(rwA)

given simpleNewtypeReadWriter[A, B](using
    newtype: Newtype.WithType[A, B],
    rwA: ReadWriter[A],
    isSimpleType: IsSimpleType[newtype.type]
): ReadWriter[B] =
  rwA.bimap[B](
    b => newtype.unwrap(b),
    a => newtype.unsafeMake(a)
  )

// Validated Subtype
given validatedSubtypeReader[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    rwA: Reader[A],
    isValidatedType: IsValidatedType[subtype.type]
): Reader[B] =
  rwA.map[B](a => subtype.make(a).fold(err => throw new upickle.core.Abort(err), identity))

given validatedSubtypeWriter[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    rwA: Writer[A]
): Writer[B] =
  subtype.unsafeMakeF(rwA)

given validatedSubtypeReadWriter[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    rwA: ReadWriter[A],
    isValidatedType: IsValidatedType[subtype.type]
): ReadWriter[B] =
  rwA.bimap[B](
    b => b,
    a => subtype.make(a).fold(err => throw new upickle.core.Abort(err), identity)
  )

// Simple Subtype
given simpleSubtypeReader[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    rwA: Reader[A],
    isSimpleType: IsSimpleType[subtype.type]
): Reader[B] =
  subtype.unsafeMakeF(rwA)

given simpleSubtypeReadWriter[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    rwA: ReadWriter[A],
    isSimpleType: IsSimpleType[subtype.type]
): ReadWriter[B] =
  subtype.unsafeMakeF(rwA)
