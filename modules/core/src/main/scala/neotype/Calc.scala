package neotype

import BinOpMatch.*

import scala.annotation.tailrec
import StringFormatting.*

import scala.quoted.*
import scala.util.matching.Regex
import CustomFromExpr.given
import neotype.Calc.closureToFunction

enum Calc[A]:
  case Value(value: A)

  // The Apply* variants represent method calls.
  // Apply0 encodes a nullary method. E.g., `string.length`
  // Apply1 encodes a unary method. E.g., `string.startsWith("a")`
  // Apply2 encodes a binary method. E.g., `string.substring(1, 2)`
  case Apply0[A, B](calc: Calc[A], op: A => B, show: String => String)                                 extends Calc[B]
  case Apply1[A, B, C](calc: Calc[A], rhs: Calc[B], op: (A, B) => C, show: (String, String) => String) extends Calc[C]
  case Apply2[A, B, C, D](
      calc: Calc[A],
      lhs: Calc[B],
      rhs: Calc[C],
      op: (A, B, C) => D,
      show: (String, String, String) => String
  ) extends Calc[D]

  case CalcApply(calc: Calc[A], args: List[Calc[?]]) extends Calc[Any]

  case CalcBlock(defs: List[CalcDef[?]], calc: Calc[A])                        extends Calc[A]
  case Reference(name: String)                                                 extends Calc[A]
  case MatchExpr(expr: Calc[A], cases: List[CalcMatchCase[A]])                 extends Calc[A]
  case IfThenElse(cond: Calc[Boolean], thenCalc: Calc[A], elseCalc: Calc[A])   extends Calc[A]
  case CalcClosure(ctx: Map[String, Any], params: List[String], body: Calc[A]) extends Calc[A]

  def render(using ctx: Map[String, String])(using Quotes): String =
    import quotes.reflect.*
    this match
      case Value(value)    => Calc.renderValue(value)
      case Reference(name) => ctx.getOrElse(name, "_")

      // Comparisons
      case Apply0(calc, _, show) => show(calc.render)
      case Apply1(lhs, rhs, _, show) =>
        report.info(s"Rendering Apply1: $lhs, $rhs")
        show(lhs.render, rhs.render)

      case Apply2(cond, lhs, rhs, _, show) => show(cond.render, lhs.render, rhs.render)

      case CalcBlock(defs, calc) =>
        val newCtx = defs.foldLeft(ctx) { (ctx, defn) =>
          defn match
            case CalcDef.CalcValDef(name, calc) =>
              ctx + (name -> calc.render(using ctx))
            case CalcDef.CalcDefDef(name, args, calc) =>
              ctx + (name -> calc.render(using
                ctx ++ args.map(a => if a.startsWith("_") then (a, "_") else (a, a))
              ))
        }
        calc.render(using newCtx)

      case MatchExpr(expr, cases) =>
        val exprStr  = expr.render
        val casesStr = indent(cases.map(_.render).mkString("\n"))
        s"match $exprStr {\n$casesStr\n}"

      case IfThenElse(cond, thenCalc, elseCalc) =>
        val condStr     = cond.render
        val thenCalcStr = thenCalc.render
        val elseCalcStr = elseCalc.render
        s"if $condStr then $thenCalcStr else $elseCalcStr"
  end render

  private def indent(str: String): String =
    str.linesIterator.map("  " + _).mkString("\n")

  def result(using context: Map[String, Any], q: Quotes): A =
    import q.reflect.*
    this match
      case Value(value)        => value
      case Reference(name)     => context(name).asInstanceOf[A]
      case Apply0(calc, op, _) => op(calc.result).asInstanceOf[A]

      case Apply1(calc, arg, op, _) =>
        arg.result match
          case closure: CalcClosure[?] =>
            val f = closureToFunction(closure)
            op.asInstanceOf[(Any, Any) => Any](calc.result, f).asInstanceOf[A]
          case result =>
            op(calc.result, result).asInstanceOf[A]

      case Apply2(calc, arg1, arg2, op, _) => op(calc.result, arg1.result, arg2.result).asInstanceOf[A]

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

      case CalcBlock(defs, calc) =>
        val newContext = defs.foldLeft(context) { (ctx, defn) =>
          defn match
            case CalcDef.CalcValDef(name, calc) =>
              ctx + (name -> calc.result(using ctx))
            case CalcDef.CalcDefDef(name, args, calc) =>
              val closure = CalcClosure(ctx, args, calc)
              ctx + (name -> closure)
        }
        calc.result(using newContext)

      case IfThenElse(cond, thenCalc, elseCalc) =>
        if cond.result then thenCalc.result else elseCalc.result

      case CalcApply(calc, args) =>
        val calcResult = calc.result
        calcResult match
          case CalcClosure(cctx, params, body) =>
            val newCtx = params.zip(args.map(_.result)).foldLeft(cctx) { (ctx, arg) =>
              ctx + arg
            }
            report.info(s"Applying closure $calcResult with args $args")
            body.result(using newCtx)
          case _ =>
            q.reflect.report.errorAndAbort(s"Cannot apply $calcResult")
            throw MatchError(calcResult)
  end result

end Calc

object Calc:
  def renderValue(value: Any): String =
    value match
      case s: String => s""""$s"""".green
      case c: Char   => s"'$c'".green
      case set: Set[?] =>
        val elems = set.map(renderValue).mkString(", ".reset)
        s"Set(".reset + elems + ")".reset
      case list: List[?] =>
        val elems = list.map(renderValue).mkString(", ".reset)
        s"List(".reset + elems + ")".reset
      case regex: Regex =>
        s"""${renderValue(regex.toString)}.r"""
      case long: Long => s"${long}L".cyan
      case _          => value.toString.cyan

  def infix(op: String)   = (a: String, b: String) => s"$a $op $b"
  def call(op: String)    = (a: String, b: String) => s"$a.$op($b)"
  def call2(op: String)   = (a: String, b: String, c: String) => s"$a.$op($b, $c)"
  def nullary(op: String) = (a: String) => s"$a.$op"
  def prefix(op: String)  = (a: String) => s"$op$a"

  def closureToFunction[A](closure: CalcClosure[A])(using Quotes): Any =
    import quotes.reflect.*
    closure match
      case CalcClosure(ctx, List(p), body) =>
        (a: Any) => body.result(using ctx ++ Map(p -> a))
      case CalcClosure(ctx, List(p1, p2), body) =>
        (a: Any, b: Any) => body.result(using ctx ++ Map(p1 -> a, p2 -> b))
      case CalcClosure(ctx, List(p1, p2, p3), body) =>
        (a: Any, b: Any, c: Any) => body.result(using ctx ++ Map(p1 -> a, p2 -> b, p3 -> c))
      case _ =>
        report.errorAndAbort(s"Closure to function for arity ${closure.params.size} not implemented")

  def unapply[A](expr: Expr[A])(using Quotes): Option[Calc[A]] =
    import quotes.reflect.*

    val calc: Option[Calc[?]] = expr.asInstanceOf[Expr[Any]] match

      // Basic Types
      case Unseal(Literal(constant))                  => Some(Calc.Value(constant.value))
      case '{ BigInt(${ Expr(string) }: String) }     => Some(Calc.Value(BigInt(string)))
      case '{ BigInt(${ Expr(int) }: Int) }           => Some(Calc.Value(BigInt(int)))
      case '{ BigInt(${ Expr(long) }: Long) }         => Some(Calc.Value(BigInt(long)))
      case '{ BigDecimal(${ Expr(string) }: String) } => Some(Calc.Value(BigDecimal(string)))
      case '{ BigDecimal(${ Expr(int) }: Int) }       => Some(Calc.Value(BigDecimal(int)))
      case '{ BigDecimal(${ Expr(long) }: Long) }     => Some(Calc.Value(BigDecimal(long)))
      case '{ BigDecimal(${ Expr(double) }: Double) } => Some(Calc.Value(BigDecimal(double)))
      case '{ (${ Expr(string) }: String).r }         => Some(Calc.Value(string.r))

      // Implicit Conversions
      case '{ int2Integer(${ Calc(int) }: Int) } => Some(int)

      // CONTAINER TYPES
      case '{ ${ CustomFromExpr.fromExprSet(set) }: Set[a] }    => Some(Calc.Value(set))
      case '{ ${ CustomFromExpr.fromExprList(list) }: List[a] } => Some(Calc.Value(list))

      case Unseal(Ident(name)) => Some(Calc.Reference(name))

      // Boolean Operations
      case '{ (${ Calc(lhs) }: Boolean) && (${ Calc(rhs) }: Boolean) } =>
        Some(Calc.Apply1(lhs, rhs, _ && _, infix("&&")))
      case '{ (${ Calc(lhs) }: Boolean) || (${ Calc(rhs) }: Boolean) } =>
        Some(Calc.Apply1(lhs, rhs, _ || _, infix("||")))
      case '{ !(${ Calc(calc) }: Boolean) } =>
        Some(Calc.Apply0(calc, !_, prefix("!")))

      // Numeric Operations
      case '{ (${ Calc(num) }: Int).toDouble } =>
        Some(Calc.Apply0(num, _.toDouble, nullary("toDouble")))

      // asInstanceOf
      case '{ (${ Calc(calc) }: a).asInstanceOf[b] } =>
        Some(Calc.Apply0(calc, _.asInstanceOf[b], nullary(s"asInstanceOf[${Type.show[b]}]")))

      // isInstanceOf
      case '{ (${ Calc(calc) }: a).isInstanceOf[b] } =>
        Some(Calc.Apply0(calc, _.isInstanceOf[b], nullary(s"isInstanceOf${Type.show[b]}")))

      // String Operations
      case '{ (${ Calc(string) }: String).length } =>
        Some(Calc.Apply0(string, _.length, nullary("length")))
      case '{ (${ Calc(string) }: String).substring(${ Calc(start) }: Int, ${ Calc(end) }: Int) } =>
        Some(Calc.Apply2(string, start, end, _.substring(_, _), call2("substring")))
      case '{ (${ Calc(string) }: String).toUpperCase } =>
        Some(Calc.Apply0(string, _.toUpperCase, nullary("toUpperCase")))
      case '{ (${ Calc(string) }: String).toLowerCase } =>
        Some(Calc.Apply0(string, _.toLowerCase, nullary("toLowerCase")))
      case '{ (${ Calc(str) }: String).startsWith(${ Calc(prefix) }: String) } =>
        Some(Calc.Apply1(str, prefix, _.startsWith(_), call("startsWith")))
      case '{ (${ Calc(str) }: String).endsWith(${ Calc(suffix) }: String) } =>
        Some(Calc.Apply1(str, suffix, _.endsWith(_), call("endsWith")))
      case '{ (${ Calc(str) }: String).contains(${ Calc(substr) }: String) } =>
        Some(Calc.Apply1(str, substr, _.contains(_), call("contains")))
      case '{ (${ Calc(str) }: String).matches(${ Calc(regex) }: String) } =>
        Some(Calc.Apply1(str, regex, _.matches(_), call("matches")))
      case '{ (${ Calc(regex) }: Regex).matches(${ Calc(str) }: String) } =>
        Some(Calc.Apply1(regex, str, _.matches(_), call("matches")))
      case '{ (${ Calc(str) }: String).nonEmpty } =>
        Some(Calc.Apply0(str, _.nonEmpty, nullary("nonEmpty")))
      case '{ (${ Calc(str) }: String).isEmpty } =>
        Some(Calc.Apply0(str, _.isEmpty, nullary("isEmpty")))
      case '{ (${ Calc(str) }: String).reverse } =>
        Some(Calc.Apply0(str, _.reverse, nullary("reverse")))
      case '{ (${ Calc(str) }: String)(${ Calc(index) }: Int) } =>
        Some(Calc.Apply1(str, index, _.apply(_), (l, r) => s"$l($r)"))
      case '{ (${ Calc(str) }: String).trim } =>
        Some(Calc.Apply0(str, _.trim, nullary("trim")))
      case '{ (${ Calc(str) }: String) ++ (${ Calc(other) }: String) } =>
        Some(Calc.Apply1(str, other, _ ++ _, infix("++")))

      case '{ scala.Predef.identity(${ Calc(calc) }) } =>
        Some(calc)
      case MatchBinEq(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, _ == _, infix("==")))
      case MatchBinNeq(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, _ != _, infix("!=")))
      case MatchBinJavaEq(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, (a, b) => a.asInstanceOf[Object] eq b.asInstanceOf[Object], infix("eq")))
      case MatchBinJavaNeq(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, (a, b) => a.asInstanceOf[Object] ne b.asInstanceOf[Object], infix("ne")))
      case MatchBinLt(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, Operations.lessThan, infix("<")))
      case MatchBinGt(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, Operations.greaterThan, infix(">")))
      case MatchBinLte(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, Operations.lessThanOrEqual, infix("<=")))
      case MatchBinGte(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, Operations.greaterThanOrEqual, infix(">=")))
      case MatchBinMinus(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, Operations.minus, infix("-")))
      case MatchBinPlus(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, Operations.plus, infix("+")))
      case MatchBinDivide(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, Operations.divide, infix("/")))
      case MatchBinMod(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, Operations.mod, infix("%")))
      case MatchBinTimes(lhs, rhs) =>
        Some(Calc.Apply1(lhs, rhs, Operations.times, infix("*")))

      case Unseal(quotes.reflect.Block(stats, Seal(Calc(expr)))) =>
        val defs = stats.collect {
          case ValDef(name, _, Some(Seal(Calc(calc)))) =>
            CalcDef.CalcValDef(name, calc)
          case DefDef(name, valDefs, _, Some(Seal(Calc(rhs)))) =>
            val params = valDefs.flatMap(_.params).collect { case ValDef(name, _, _) => name }
            CalcDef.CalcDefDef(name, params, rhs)
        }
        Some(Calc.CalcBlock(defs, expr))

      // Match Expressions
      case Unseal(Match(Seal(Calc(expr)), caseDefs)) =>
        val calcCaseDefs = caseDefs.map(CalcMatchCase.parse)
        Some(MatchExpr(expr, calcCaseDefs))

      case '{
            if ${ Calc(cond) } then ${ Calc(trueCase) }
            else ${ Calc(falseCase) }
          } =>
        Some(Calc.IfThenElse(cond, trueCase, falseCase))

      // Remove Typed Nodes
      case Unseal(Typed(t, _)) =>
        unapply(t.asExpr)

      // Remove Inlined Nodes
      case Unseal(Uninlined(t)) =>
        unapply(t.asExpr)

      // Set Operations (+ and - are handled by the MatchBinPlus and MatchBinMinus cases)
      case '{ type a; (${ Calc(set) }: Set[`a`]).contains(${ Calc(elem) }: `a`) } =>
        Some(Calc.Apply1(set, elem, _.contains(_), call("contains")))
      case '{ type a; (${ Calc(set) }: Set[`a`]).intersect(${ Calc(other) }: Set[`a`]) } =>
        Some(Calc.Apply1(set, other, _.intersect(_), call("intersect")))
      case '{ type a; (${ Calc(set) }: Set[`a`]).union(${ Calc(other) }: Set[`a`]) } =>
        Some(Calc.Apply1(set, other, _.union(_), call("union")))
      case '{ type a; (${ Calc(set) }: Set[`a`]).diff(${ Calc(other) }: Set[`a`]) } =>
        Some(Calc.Apply1(set, other, _.diff(_), call("diff")))
      case '{ type a; (${ Calc(set) }: Set[`a`]).++(${ Calc(other) }: Set[`a`]) } =>
        Some(Calc.Apply1(set, other, _.++(_), infix("++")))
      case '{ type a; (${ Calc(set) }: Set[`a`]).--(${ Calc(other) }: Set[`a`]) } =>
        Some(Calc.Apply1(set, other, _.--(_), infix("--")))
      case '{ type a; (${ Calc(set) }: Set[`a`]).isEmpty } =>
        Some(Calc.Apply0(set, _.isEmpty, nullary("isEmpty")))
      case '{ type a; (${ Calc(set) }: Set[`a`]).subsetOf(${ Calc(other) }: Set[`a`]) } =>
        Some(Calc.Apply1(set, other, _.subsetOf(_), call("subsetOf")))
      case '{ type a; (${ Calc(set) }: Set[`a`]).apply(${ Calc(elem) }: `a`) } =>
        Some(Calc.Apply1(set, elem, _.apply(_), (a, b) => s"$a($b)"))

      // List Operations
      case '{ type a; (${ Calc(list) }: List[`a`]).:+(${ Calc(elem) }: `a`) } =>
        Some(Calc.Apply1(list, elem, _ :+ _, infix(":+")))
      case '{ type a; (${ Calc(list) }: List[`a`]).::[`a`](${ Calc(elem) }: `a`) } =>
        Some(Calc.Apply1(elem, list, _ :: _, infix("::")))
      case '{ type a; (${ Calc(list) }: List[`a`]).:::[`a`](${ Calc(other) }: List[`a`]) } =>
        Some(Calc.Apply1(other, list, _ ::: _, infix(":::")))
      case '{ type a; (${ Calc(list) }: List[`a`]).head } =>
        Some(Calc.Apply0(list, _.head, nullary("head")))
      case '{ type a; (${ Calc(list) }: List[`a`]).tail } =>
        Some(Calc.Apply0(list, _.tail, nullary("tail")))
      case '{ type a; (${ Calc(list) }: List[`a`]).isEmpty } =>
        Some(Calc.Apply0(list, _.isEmpty, nullary("isEmpty")))
      case '{ type a; (${ Calc(list) }: List[`a`]).nonEmpty } =>
        Some(Calc.Apply0(list, _.nonEmpty, nullary("nonEmpty")))
      case '{ type a; (${ Calc(list) }: List[`a`]).size } =>
        Some(Calc.Apply0(list, _.size, nullary("size")))
      case '{ type a; (${ Calc(list) }: List[`a`]).length } =>
        Some(Calc.Apply0(list, _.length, nullary("length")))
      case '{ type a; (${ Calc(list) }: List[`a`]).contains(${ Calc(elem) }: `a`) } =>
        Some(Calc.Apply1(list, elem, _.contains(_), call("contains")))
      case '{ type a; (${ Calc(list) }: List[`a`]).apply(${ Calc(index) }: Int) } =>
        Some(Calc.Apply1(list, index, _.apply(_), infix("apply")))
      case '{ type a; (${ Calc(list) }: List[`a`]).filter(${ Calc(predicate) }: `a` => Boolean) } =>
        Some(Calc.Apply1(list, predicate, _.filter(_), call("filter")))
      case '{ type a; type b; (${ Calc(list) }: List[`a`]).map(${ Calc(f) }: `a` => `b`) } =>
        Some(Calc.Apply1(list, f, _.map(_), call("map")))

      case Unseal(Closure(Ident(name), _)) =>
        Some(Calc.Reference(name))

      case Unseal(Apply(Ident(name), List(Seal(Calc(arg))))) =>
        Some(Calc.CalcApply(Calc.Reference(name), List(arg)))

      case '{ type a; type b; (${ Calc(f) }: `a` => `b`)(${ Calc(a) }: `a`) } =>
        Some(Calc.CalcApply(f, List(a)))

      case other =>
        // DEV MODE
//        report.errorAndAbort(
//          s"CALC PARSE FAIL: ${other.show}\n${other.asTerm.tpe.show}\n${other.asTerm.underlyingArgument}"
//        )
        None

    calc.asInstanceOf[Option[Calc[A]]]

enum CalcDef[A]:
  case CalcValDef(name: String, calc: Calc[A])
  case CalcDefDef(name: String, params: List[String], calc: Calc[A])

case class CalcMatchCase[A](pattern: CalcPattern[A], guard: Option[Calc[Boolean]], calc: Calc[A]):
  def render(using Map[String, String])(using Quotes): String =
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
      case CaseDef(pattern, guard, Seal(Calc(calc))) =>
        val guardCalc = guard.map { case Seal(Calc[Boolean](guardCalc)) => guardCalc }
        CalcMatchCase(CalcPattern.parse(pattern), guardCalc, calc)
      case other =>
        report.errorAndAbort(s"CalcMatchCase parse failed to parse: ${other}")
        ???

enum CalcPattern[A]:
  case Value(value: Calc[A])
  case Variable(name: String)
  case Alternative(patterns: List[CalcPattern[A]])
  case Wildcard()

  def bindings: List[String] =
    this match
      case Value(_)       => Nil
      case Variable(name) => List(name)
      case Wildcard()     => Nil
      case Alternative(patterns) =>
        patterns.flatMap(_.bindings)

  def render(using Map[String, String])(using Quotes): String =
    this match
      case Value(value)   => value.render
      case Variable(name) => name
      case Wildcard()     => "_"
      case Alternative(patterns) =>
        patterns.map(_.render).mkString("(", " | ", ")")

  def matches(value: Any)(using Map[String, Any], Quotes): Boolean =
    this match
      case Value(value) => value.result == value
      case Variable(_)  => true
      case Wildcard()   => true
      case Alternative(patterns) =>
        patterns.exists(_.matches(value))

object CalcPattern:
  def parse(using Quotes)(term: quotes.reflect.Tree): CalcPattern[Any] =
    import quotes.reflect as r
    term match
      case r.Wildcard()               => CalcPattern.Wildcard()
      case r.Bind(name, r.Wildcard()) => CalcPattern.Variable(name)
      case Seal(Calc(value))          => CalcPattern.Value(value)
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
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[Expr[Any]] =
    import quotes.reflect.*
    Some(term.asExpr)
