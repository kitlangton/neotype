package neotype.zio

import neotype.*
import _root_.zio.*

// Newtype
given [A, B](using newType: Newtype.WithType[A, B], tag: Tag[A]): Tag[B] =
  newType.unsafeWrapF(tag)

// Newtype.Simple
given [A, B](using newType: Newtype.Simple.WithType[A, B], tag: Tag[A]): Tag[B] =
  newType.unsafeWrapF(tag)

// Subtype
given [A, B <: A](using subType: Subtype.WithType[A, B], tag: Tag[A]): Tag[B] =
  subType.unsafeWrapF(tag)

// Subtype.Simple
given [A, B <: A](using subType: Subtype.Simple.WithType[A, B], tag: Tag[A]): Tag[B] =
  subType.unsafeWrapF(tag)
