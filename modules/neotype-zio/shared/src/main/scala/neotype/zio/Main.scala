package neotype.zio

import neotype.*
import _root_.zio.*

// Newtype
given [A, B](using newType: Newtype.WithType[A, B], tag: Tag[A]): Tag[B] =
  newType.unsafeMakeF(tag)

// Subtype
given [A, B <: A](using subType: Subtype.WithType[A, B], tag: Tag[A]): Tag[B] =
  subType.unsafeMakeF(tag)
