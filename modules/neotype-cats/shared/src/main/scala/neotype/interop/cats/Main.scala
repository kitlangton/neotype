package neotype.interop.cats

import cats.Eq
import cats.Order
import cats.Show
import neotype.Newtype
import neotype.Subtype

/////////////
// NEWTYPE //
/////////////

given newtypeShow[A, B](using newtype: Newtype.WithType[A, B], showA: Show[A]): Show[B] =
  (b: B) => showA.show(newtype.unwrap(b))

given newtypeEq[A, B](using newtype: Newtype.WithType[A, B], eqA: Eq[A]): Eq[B] =
  Eq.instance((b1, b2) => eqA.eqv(newtype.unwrap(b1), newtype.unwrap(b2)))

given newtypeOrder[A, B](using newtype: Newtype.WithType[A, B], orderA: Order[A]): Order[B] =
  Order.by(newtype.unwrap)

/////////////
// SUBTYPE //
/////////////

given subtypeShow[A, B <: A](using subtype: Subtype.WithType[A, B], showA: Show[A]): Show[B] =
  (b: B) => showA.show(b)

given subtypeEq[A, B <: A](using subtype: Subtype.WithType[A, B], eqA: Eq[A]): Eq[B] =
  Eq.instance((b1, b2) => eqA.eqv(b1, b2))

given subtypeOrder[A, B <: A](using subtype: Subtype.WithType[A, B], orderA: Order[A]): Order[B] =
  Order.by[B, A](identity)
