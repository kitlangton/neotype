package comptime

sealed trait Eval

object Eval:
  final case class Value(value: Any)                                                          extends Eval
  final case class Apply1(left: Eval, right: Eval, f: (Any, Any) => Any)                      extends Eval
  final case class Apply2(left: Eval, mid: Eval, right: Eval, f: (Any, Any, Any) => Any)      extends Eval
  final case class Apply3(a: Eval, b: Eval, c: Eval, d: Eval, f: (Any, Any, Any, Any) => Any) extends Eval
  final case class ApplyByName1(left: Eval, right: Eval, f: (Any, () => Any) => Any)          extends Eval
  final case class BuildList(elems: List[Eval], build: List[Any] => Any)                      extends Eval
  final case class If(cond: Eval, onTrue: Eval, onFalse: Eval)                                extends Eval

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
