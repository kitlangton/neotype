package neotype.caliban

import _root_.caliban.CalibanError.ExecutionError
import caliban.schema.*
import neotype.*

// Newtype
given [R, A, B](using newType: Newtype.WithType[A, B], schema: Schema[R, A]): Schema[R, B] =
  schema.contramap(newType.unwrap)

given [A, B](using newType: Newtype.WithType[A, B], argBuilder: ArgBuilder[A]): ArgBuilder[B] =
  argBuilder.flatMap(newType.make(_).left.map(ExecutionError(_)))

// Subtype
given [R, A, B <: A](using subType: Subtype.WithType[A, B], schema: Schema[R, A]): Schema[R, B] =
  schema.contramap(identity)

given [A, B <: A](using subType: Subtype.WithType[A, B], argBuilder: ArgBuilder[A]): ArgBuilder[B] =
  argBuilder.flatMap(subType.make(_).left.map(ExecutionError(_)))
