package comptime

sealed trait Eval

// Mutable reference cell for var support
final class MutableRef(var value: Any)

object Eval:
  final case class Value(value: Any)                                                          extends Eval
  final case class Apply1(left: Eval, right: Eval, f: (Any, Any) => Any)                      extends Eval
  final case class Apply2(left: Eval, mid: Eval, right: Eval, f: (Any, Any, Any) => Any)      extends Eval
  final case class Apply3(a: Eval, b: Eval, c: Eval, d: Eval, f: (Any, Any, Any, Any) => Any) extends Eval
  final case class ApplyByName1(left: Eval, right: Eval, f: (Any, () => Any) => Any)          extends Eval
  final case class BuildList(elems: List[Eval], build: List[Any] => Any)                      extends Eval
  final case class If(cond: Eval, onTrue: Eval, onFalse: Eval)                                extends Eval
  // Sequence two evals - run first, then second, return second's result
  final case class Seq(first: Eval, second: Eval)                                             extends Eval
  // Read from a mutable ref
  final case class ReadRef(ref: MutableRef)                                                   extends Eval
  // Write to a mutable ref, returns Unit
  final case class WriteRef(ref: MutableRef, value: Eval)                                     extends Eval
  // Marker for var bindings in environment - holds the MutableRef
  final case class VarBinding(ref: MutableRef)                                                extends Eval

  def run(eval: Eval): Any =
    eval match
      case Value(value)                => value
      case Apply1(left, right, f)      => f(run(left), run(right))
      case Apply2(left, mid, right, f) => f(run(left), run(mid), run(right))
      case Apply3(a, b, c, d, f)       => f(run(a), run(b), run(c), run(d))
      case ApplyByName1(left, right, f) =>
        val a            = run(left)
        def thunk(): Any = run(right)
        f(a, thunk)
      case BuildList(elems, build) =>
        build(elems.map(run))
      case If(cond, onTrue, onFalse) =>
        if run(cond).asInstanceOf[Boolean] then run(onTrue) else run(onFalse)
      case Seq(first, second) =>
        run(first)
        run(second)
      case ReadRef(ref) =>
        ref.value
      case WriteRef(ref, value) =>
        ref.value = run(value)
        ()
      case VarBinding(ref) =>
        ref.value
