package neotype.interop.cats

import cats.Eq
import cats.Order
import cats.Show
import neotype.WrappedType

given show[A, B](using nt: WrappedType[A, B], showA: Show[A]): Show[B] =
  (b: B) => showA.show(nt.unwrap(b))

given eq[A, B](using nt: WrappedType[A, B], eqA: Eq[A]): Eq[B] =
  Eq.instance((b1, b2) => eqA.eqv(nt.unwrap(b1), nt.unwrap(b2)))

given order[A, B](using nt: WrappedType[A, B], orderA: Order[A]): Order[B] =
  Order.by(nt.unwrap)
