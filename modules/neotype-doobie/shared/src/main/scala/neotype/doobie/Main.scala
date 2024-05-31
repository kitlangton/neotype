package neotype.doobie

import cats.Show
import doobie.*
import neotype.*

// Newtype

given newtypeGet[A, B](using newtype: Newtype.WithType[A, B], get: Get[A], show: Show[A]): Get[B] =
  get.temap(newtype.make(_))

given newtypePut[A, B](using newtype: Newtype.WithType[A, B], put: Put[A]): Put[B] =
  put.asInstanceOf[Put[B]]

// Subtype

given subtypeGet[A, B <: A](using subtype: Subtype.WithType[A, B], get: Get[A], show: Show[A]): Get[B] =
  get.temap(subtype.make(_))

given subtypePut[A, B <: A](using subtype: Subtype.WithType[A, B], put: Put[A]): Put[B] =
  put.asInstanceOf[Put[B]]
