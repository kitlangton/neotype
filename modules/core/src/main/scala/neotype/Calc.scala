package neotype

import scala.annotation.tailrec
import StringFormatting.*
import scala.quoted.*
import scala.util.matching.Regex
import CustomFromExpr.given

enum Calc[A]:
  case Constant(value: A)

  // Comparisons
  case BinaryOp[A, B, C](lhs: Calc[A], rhs: Calc[B], op: (A, B) => C, render: (String, String) => String)
      extends Calc[C]
  case UnaryOp[A, B](calc: Calc[A], op: A => B, render: String => String) extends Calc[B]
  case TernaryOp[A, B, C, D](
      cond: Calc[A],
      lhs: Calc[B],
      rhs: Calc[C],
      op: (A, B, C) => D,
      render: (String, String, String) => String
  ) extends Calc[D]

  // Custom
  case Block(defs: List[CalcValDef[?]], calc: Calc[A]) extends Calc[A]
  case Reference[A](name: String)                      extends Calc[A]

  case MatchExpr[A](expr: Calc[A], cases: List[CalcMatchCase[A]]) extends Calc[A]

  def render(using ctx: Map[String, String]): String =
    this match
      case Constant(value) => Calc.renderConstant(value)

      // Comparisons
      case UnaryOp(calc, _, show)             => show(calc.render)
      case BinaryOp(lhs, rhs, _, show)        => show(lhs.render, rhs.render)
      case TernaryOp(cond, lhs, rhs, _, show) => show(cond.render, lhs.render, rhs.render)

      case Block(defs, calc) =>
        val newCtx = defs.foldLeft(ctx) { (ctx, defn) =>
          ctx + (defn.name -> defn.calc.render(using ctx))
        }
        calc.render(using newCtx)

      case Reference(name) => ctx(name)

      case MatchExpr(expr, cases) =>
        val exprStr  = expr.render
        val casesStr = indent(cases.map(_.render).mkString("\n"))
        s"match $exprStr {\n$casesStr\n}"

  def indent(str: String): String =
    str.linesIterator.map("  " + _).mkString("\n")

  def result(using context: Map[String, Any], q: Quotes): A =
    this match
      case Constant(value)           => value
      case Reference(name)           => context(name).asInstanceOf[A]
      case UnaryOp(calc, op, _)      => op(calc.result).asInstanceOf[A]
      case BinaryOp(lhs, rhs, op, _) => op(lhs.result, rhs.result).asInstanceOf[A]
      case TernaryOp(a, b, c, op, _) => op(a.result, b.result, c.result).asInstanceOf[A]

      case MatchExpr(expr, cases) =>
        val exprResult = expr.result
        cases.find(_.matches(exprResult)) match
          case Some(CalcMatchCase(pattern, _, calc)) =>
            val newCtx = pattern.bindings.foldLeft(context) { (ctx, name) =>
              ctx + (name -> exprResult)
            }
            calc.result(using newCtx)
          case None =>
            q.reflect.report.errorAndAbort(s"CalcMatchCase not found for $exprResult")
            throw MatchError(exprResult)

      case Block(defs, calc) =>
        val newContext = defs.foldLeft(context) { (ctx, defn) =>
          ctx + (defn.name -> defn.calc.result(using ctx))
        }
        calc.result(using newContext)
  end result
end Calc

case class BinaryOpMatch(name: String):
  def unapply(using Quotes)(expr: Expr[Any]): Option[(Calc[Any], Calc[Any])] =
    import quotes.reflect.*
    expr.asTerm match
      case Apply(Select(lhs, `name`), List(rhs)) =>
        (lhs.asExpr, rhs.asExpr) match
          case (Calc[Any](lhs), Calc[Any](rhs)) =>
            Some((lhs, rhs))
      case _ =>
        None

val MatchBinEq     = BinaryOpMatch("==")
val MatchBinLt     = BinaryOpMatch("<")
val MatchBinGt     = BinaryOpMatch(">")
val MatchBinLte    = BinaryOpMatch("<=")
val MatchBinGte    = BinaryOpMatch(">=")
val MatchBinMinus  = BinaryOpMatch("-")
val MatchBinPlus   = BinaryOpMatch("+")
val MatchBinTimes  = BinaryOpMatch("*")
val MatchBinDivide = BinaryOpMatch("/")
val MatchBinMod    = BinaryOpMatch("%")

object Calc:
  def renderConstant(value: Any): String =
    value match
      case s: String => s""""$s"""".green
      case c: Char   => s"'$c'".green
      case set: Set[?] =>
        val elems = set.map(renderConstant).mkString(", ".reset)
        s"Set(".reset + elems + ")".reset
      case list: List[?] =>
        val elems = list.map(renderConstant).mkString(", ".reset)
        s"List(".reset + elems + ")".reset
      case regex: Regex =>
        s"""${renderConstant(regex.toString)}.r"""
      case long: Long => s"${long}L".cyan
      case _          => value.toString.cyan

  def infix(op: String)   = (a: String, b: String) => s"$a $op $b"
  def call(op: String)    = (a: String, b: String) => s"$a.$op($b)"
  def call2(op: String)   = (a: String, b: String, c: String) => s"$a.$op($b, $c)"
  def nullary(op: String) = (a: String) => s"$a.$op"
  def prefix(op: String)  = (a: String) => s"$op$a"

  def unapply[A](expr: Expr[Any])(using Quotes): Option[Calc[A]] =
    import quotes.reflect.*
    import quotes.reflect as r
    val result: Option[Calc[?]] = expr match
      // BASIC TYPES
      case Unseal(r.Literal(constant))                => Some(Calc.Constant(constant.value))
      case '{ BigInt(${ Expr(string) }: String) }     => Some(Calc.Constant(BigInt(string)))
      case '{ BigDecimal(${ Expr(string) }: String) } => Some(Calc.Constant(BigDecimal(string)))
      case '{ (${ Expr(string) }: String).r }         => Some(Calc.Constant(string.r))

      // CONTAINER TYPES
      case '{ type a; ${ Expr[Set[`a`]](set) }: Set[`a`] }    => Some(Calc.Constant(set))
      case '{ type a; ${ Expr[List[`a`]](list) }: List[`a`] } => Some(Calc.Constant(list))

      case Unseal(Ident(name)) => Some(Calc.Reference(name))

      // Boolean Operations
      case '{ (${ Calc[Boolean](lhs) }: Boolean) && (${ Calc[Boolean](rhs) }: Boolean) } =>
        Some(Calc.BinaryOp(lhs, rhs, _ && _, infix("&&")))
      case '{ (${ Calc[Boolean](lhs) }: Boolean) || (${ Calc[Boolean](rhs) }: Boolean) } =>
        Some(Calc.BinaryOp(lhs, rhs, _ || _, infix("||")))
      case '{ !(${ Calc[Boolean](calc) }: Boolean) } =>
        Some(Calc.UnaryOp(calc, !_, prefix("!")))

      // Numeric Operations
      case '{ (${ Calc[Int](num) }: Int).toDouble } =>
        Some(Calc.UnaryOp(num, _.toDouble, nullary("toDouble")))

      // String Operations
      case '{ (${ Calc[String](string) }: String).length } =>
        Some(Calc.UnaryOp(string, _.length, nullary("length")))
      case '{ (${ Calc[String](string) }: String).substring(${ Calc[Int](start) }: Int, ${ Calc[Int](end) }: Int) } =>
        Some(Calc.TernaryOp(string, start, end, _.substring(_, _), call2("substring")))
      case '{ (${ Calc[String](string) }: String).toUpperCase } =>
        Some(Calc.UnaryOp(string, _.toUpperCase, nullary("toUpperCase")))
      case '{ (${ Calc[String](string) }: String).toLowerCase } =>
        Some(Calc.UnaryOp(string, _.toLowerCase, nullary("toLowerCase")))
      case '{ (${ Calc[String](str) }: String).startsWith(${ Calc[String](prefix) }: String) } =>
        Some(Calc.BinaryOp(str, prefix, _.startsWith(_), call("startsWith")))
      case '{ (${ Calc[String](str) }: String).endsWith(${ Calc[String](suffix) }: String) } =>
        Some(Calc.BinaryOp(str, suffix, _.endsWith(_), call("endsWith")))
      case '{ (${ Calc[String](str) }: String).contains(${ Calc[String](substr) }: String) } =>
        Some(Calc.BinaryOp(str, substr, _.contains(_), call("contains")))
      case '{ (${ Calc[String](str) }: String).matches(${ Calc[String](regex) }: String) } =>
        Some(Calc.BinaryOp(str, regex, _.matches(_), call("matches")))
      case '{ (${ Calc[Regex](regex) }: Regex).matches(${ Calc[String](str) }: String) } =>
        Some(Calc.BinaryOp(regex, str, _.matches(_), call("matches")))
      case '{ (${ Calc[String](str) }: String).nonEmpty } =>
        Some(Calc.UnaryOp(str, _.nonEmpty, nullary("nonEmpty")))
      case '{ (${ Calc[String](str) }: String).isEmpty } =>
        Some(Calc.UnaryOp(str, _.isEmpty, nullary("isEmpty")))
      case '{ (${ Calc[String](str) }: String)(${ Calc[Int](index) }: Int) } =>
        Some(Calc.BinaryOp(str, index, _.apply(_), (l, r) => s"$l($r)"))
      case '{ (${ Calc[String](str) }: String).trim } =>
        Some(Calc.UnaryOp(str, _.trim, nullary("trim")))
      case '{ scala.Predef.identity[a](${ Calc[Any](calc) }) } =>
        Some(calc)
      case MatchBinEq(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, _ == _, infix("==")))
      case MatchBinLt(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.lessThan, infix("<")))
      case MatchBinGt(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.greaterThan, infix(">")))
      case MatchBinLte(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.lessThanOrEqual, infix("<=")))
      case MatchBinGte(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.greaterThanOrEqual, infix(">=")))
      case MatchBinMinus(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.minus, infix("-")))
      case MatchBinPlus(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.plus, infix("+")))
      case MatchBinDivide(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.divide, infix("/")))
      case MatchBinMod(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.mod, infix("%")))
      case MatchBinTimes(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.times, infix("*")))

      case Unseal(quotes.reflect.Block(stats, Seal(Calc[Any](expr)))) =>
        val defs = stats.collect { case ValDef(name, _, Some(Seal(Calc[Any](calc)))) =>
          CalcValDef(name, calc)
        }
        Some(Calc.Block(defs, expr))

      // parse match expression
      case Unseal(Match(Seal(Calc[Any](expr)), caseDefs)) =>
        val calcCaseDefs = caseDefs.map(CalcMatchCase.parse)
        Some(MatchExpr(expr, calcCaseDefs))

      case Unseal(Typed(t, _)) =>
        unapply(t.asExpr)

      case Unseal(Uninlined(t)) =>
        unapply(t.asExpr)

      // Set Operations
      case '{ type a; (${ Calc[Set[`a`]](set) }: Set[`a`]).contains(${ Calc[`a`](elem) }: `a`) } =>
        Some(Calc.BinaryOp(set, elem, _.contains(_), call("contains")))

      // List Operations
      case '{ type a; (${ Calc[List[`a`]](list) }: List[`a`]).:+(${ Calc[`a`](elem) }: `a`) } =>
        Some(Calc.BinaryOp(list, elem, _ :+ _, infix(":+")))
      case '{ type a; (${ Calc[List[`a`]](list) }: List[`a`]).::[`a`](${ Calc[`a`](elem) }: `a`) } =>
        Some(Calc.BinaryOp(elem, list, _ :: _, infix("::")))

      // Fixing
      case other =>
//        report.errorAndAbort(
//          s"CALC PARSE FAIL: ${other.show}\n${other.asTerm.tpe.show}\n${other.asTerm.underlyingArgument}"
//        )
        None

    result.asInstanceOf[Option[Calc[A]]]

case class CalcValDef[A](name: String, calc: Calc[A])

case class CalcMatchCase[A](pattern: CalcPattern[A], guard: Option[Calc[Boolean]], calc: Calc[A]):
  def render(using Map[String, String]): String =
    s"${pattern.render} => ${calc.render}"

  def matches(using ctx: Map[String, Any], q: Quotes)(value: Any): Boolean =
    val patternMatches = pattern.matches(value)
    lazy val guardResult = guard
      .map { calc =>
        val newMap = ctx ++ pattern.bindings.map(_ -> value)
        calc.result(using newMap)
      }
      .getOrElse(true)
    patternMatches && guardResult

object CalcMatchCase:
  def parse(using Quotes)(caseDef: quotes.reflect.CaseDef): CalcMatchCase[Any] =
    import quotes.reflect.*
    caseDef match
      case CaseDef(pattern, guard, Seal(Calc[Any](calc))) =>
        val guardCalc = guard.map { case Seal(Calc[Boolean](guardCalc)) => guardCalc }
        CalcMatchCase(CalcPattern.parse(pattern), guardCalc, calc)
      case other =>
        report.errorAndAbort(s"CalcMatchCase parse failed to parse: ${other}")
        ???

enum CalcPattern[A]:
  case Constant(value: Calc[A])
  case Variable(name: String)
  case Alternative(patterns: List[CalcPattern[A]])
  case Wildcard()

  def bindings: List[String] =
    this match
      case Constant(_)    => Nil
      case Variable(name) => List(name)
      case Wildcard()     => Nil
      case Alternative(patterns) =>
        patterns.flatMap(_.bindings)

  def render(using Map[String, String]): String =
    this match
      case Constant(constant) => constant.render
      case Variable(name)     => name
      case Wildcard()         => "_"
      case Alternative(patterns) =>
        patterns.map(_.render).mkString("(", " | ", ")")

  def matches(value: Any)(using Map[String, Any], Quotes): Boolean =
    this match
      case Constant(constant) => constant.result == value
      case Variable(_)        => true
      case Wildcard()         => true
      case Alternative(patterns) =>
        patterns.exists(_.matches(value))

object CalcPattern:
  def parse(using Quotes)(term: quotes.reflect.Tree): CalcPattern[Any] =
    import quotes.reflect as r
    term match
      case r.Wildcard()               => CalcPattern.Wildcard()
      case r.Bind(name, r.Wildcard()) => CalcPattern.Variable(name)
      case Seal(Calc[Any](constant))  => CalcPattern.Constant(constant)
      case r.Alternatives(patterns)   => CalcPattern.Alternative(patterns.map(parse))

object Unseal:
  def unapply(expr: Expr[?])(using Quotes): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    Some(expr.asTerm)

object Uninlined:
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    term match
      case Inlined(_, bindings, t) => Some(quotes.reflect.Block(bindings, t))
      case t                       => None

object Seal:
  // turn term into expr
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[Expr[Any]] =
    import quotes.reflect.*
    Some(term.asExpr)
