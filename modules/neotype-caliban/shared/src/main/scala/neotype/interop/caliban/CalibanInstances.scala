package neotype.interop.caliban

import _root_.caliban.CalibanError.ExecutionError
import caliban.schema.*
import neotype.*

given [R, A, B](using nt: WrappedType[A, B], schema: Schema[R, A]): Schema[R, B] =
  schema.contramap(nt.unwrap)

given [A, B](using nt: WrappedType[A, B], argBuilder: ArgBuilder[A]): ArgBuilder[B] =
  argBuilder.flatMap(nt.make(_).left.map(ExecutionError(_)))
