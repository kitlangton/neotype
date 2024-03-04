package neotype.eval

import BinOpMatch.*

import scala.annotation.tailrec
import neotype.StringFormatting.*

import scala.quoted.*
import scala.util.matching.Regex
import CustomFromExpr.given
import neotype.eval.Eval.closureToFunction
import java.util as ju

enum EvalError extends Throwable:
  case MissingReference(name: String) extends EvalError

enum Eval[A]:
  case Value(value: A)

  // The Apply* variants represent method calls.
  // Apply0 encodes a nullary method. E.g., `string.length`
  // Apply1 encodes a unary method. E.g., `string.startsWith("a")`
  // Apply2 encodes a binary method. E.g., `string.substring(1, 2)`
  case Apply0[A, B](eval: Eval[A], op: A => B, show: String => String)                                 extends Eval[B]
  case Apply1[A, B, C](eval: Eval[A], rhs: Eval[B], op: (A, B) => C, show: (String, String) => String) extends Eval[C]
  case Apply2[A, B, C, D](
      eval: Eval[A],
      lhs: Eval[B],
      rhs: Eval[C],
      op: (A, B, C) => D,
      show: (String, String, String) => String
  ) extends Eval[D]

  case EvalApply(eval: Eval[A], args: List[Eval[?]]) extends Eval[Any]

  case EvalStringContext(parts: List[String], args: List[Eval[?]]) extends Eval[String]

  case EvalBlock(defs: List[EvalDef[?]], eval: Eval[A])                        extends Eval[A]
  case Reference(name: String)                                                 extends Eval[A]
  case MatchExpr(expr: Eval[A], cases: List[EvalMatchCase[A]])                 extends Eval[A]
  case IfThenElse(cond: Eval[Boolean], thenEval: Eval[A], elseEval: Eval[A])   extends Eval[A]
  case EvalClosure(ctx: Map[String, Any], params: List[String], body: Eval[A]) extends Eval[A]

  def render(using ctx: Map[String, String])(using Quotes): String =
    import quotes.reflect.*
    this match
      case Value(value)    => Eval.renderValue(value)
      case Reference(name) => ctx.getOrElse(name, "_")

      // Comparisons
      case Apply0(eval, _, show)     => show(eval.render)
      case Apply1(lhs, rhs, _, show) => show(lhs.render, rhs.render)

      case Apply2(cond, lhs, rhs, _, show) => show(cond.render, lhs.render, rhs.render)

      case EvalStringContext(parts, args) =>
        val partsStr = parts.map(s => s.green)
        val argsStr = args.map(_.render).map {
          case s: String => s"""$${$s}"""
          case other     => other
        }
        s"""s"${StringContext(partsStr*).s(argsStr*)}""""

      case EvalBlock(defs, eval) =>
        val newCtx = defs.foldLeft(ctx) { (ctx, defn) =>
          defn match
            case EvalDef.EvalValDef(name, eval) =>
              ctx + (name -> eval.render(using ctx))
            case EvalDef.EvalDefDef(name, args, eval) =>
              ctx + (name -> eval.render(using
                ctx ++ args.map(a => if a.startsWith("_") then (a, "_") else (a, a))
              ))
        }
        eval.render(using newCtx)

      case MatchExpr(expr, cases) =>
        val exprStr  = expr.render
        val casesStr = indent(cases.map(_.render).mkString("\n"))
        s"match $exprStr {\n$casesStr\n}"

      case IfThenElse(cond, thenEval, elseEval) =>
        val condStr     = cond.render
        val thenEvalStr = thenEval.render
        val elseEvalStr = elseEval.render
        s"if $condStr then $thenEvalStr else $elseEvalStr"
  end render

  private def indent(str: String): String =
    str.linesIterator.map("  " + _).mkString("\n")

  def result(using context: Map[String, Any], q: Quotes): A =
    import q.reflect.*
    this match
      case Value(value) => value
      case Reference(name) =>
        context
          .getOrElse(
            name,
            throw EvalError.MissingReference(name)
          )
          .asInstanceOf[A]

      case Apply0(eval, op, _) =>
        op(eval.result).asInstanceOf[A]

      case Apply1(eval, arg, op, _) =>
        arg.result match
          case closure: EvalClosure[?] =>
            val f = closureToFunction(closure)
            op.asInstanceOf[(Any, Any) => Any](eval.result, f).asInstanceOf[A]
          case result =>
            op(eval.result, result).asInstanceOf[A]

      case Apply2(eval, arg1, arg2, op, _) => op(eval.result, arg1.result, arg2.result).asInstanceOf[A]

      case MatchExpr(expr, cases) =>
        val exprResult = expr.result
        cases.find(_.matches(exprResult)) match
          case Some(EvalMatchCase(pattern, _, eval)) =>
            val newCtx = pattern.bindings.foldLeft(context) { (ctx, name) =>
              ctx + (name -> exprResult)
            }
            eval.result(using newCtx)
          case None =>
            q.reflect.report.errorAndAbort(s"EvalMatchCase not found for $exprResult")
            throw MatchError(exprResult)

      case EvalStringContext(parts, args) =>
        val argsStr = args.map(_.result(using context))
        StringContext(parts*).s(argsStr*)

      case EvalBlock(defs, eval) =>
        val newContext = defs.foldLeft(context) { (ctx, defn) =>
          defn match
            case EvalDef.EvalValDef(name, eval) =>
              ctx + (name -> eval.result(using ctx))
            case EvalDef.EvalDefDef(name, args, eval) =>
              val closure = EvalClosure(ctx, args, eval)
              ctx + (name -> closure)
        }
        eval.result(using newContext)

      case IfThenElse(cond, thenEval, elseEval) =>
        if cond.result then thenEval.result else elseEval.result

      case EvalApply(eval, args) =>
        val calcResult = eval.result

        calcResult match
          case EvalClosure(cctx, params, body) =>
            val newCtx = params.zip(args.map(_.result)).foldLeft(cctx) { (ctx, arg) =>
              ctx + arg
            }
            report.info(s"Applying closure $calcResult with args $args")
            body.result(using newCtx)
          case f: Function1[Any, Any] =>
            f.apply(args.toList)
          case _ =>
            q.reflect.report.errorAndAbort(s"Cannot apply $calcResult")
            throw MatchError(calcResult)
  end result

end Eval

object Eval:
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

  def closureToFunction[A](closure: EvalClosure[A])(using Quotes): Any =
    import quotes.reflect.*
    closure match
      case EvalClosure(ctx, List(p), body) =>
        (a: Any) => body.result(using ctx ++ Map(p -> a))
      case EvalClosure(ctx, List(p1, p2), body) =>
        (a: Any, b: Any) => body.result(using ctx ++ Map(p1 -> a, p2 -> b))
      case EvalClosure(ctx, List(p1, p2, p3), body) =>
        (a: Any, b: Any, c: Any) => body.result(using ctx ++ Map(p1 -> a, p2 -> b, p3 -> c))
      case _ =>
        report.errorAndAbort(s"Closure to function for arity ${closure.params.size} not implemented")

  def unapply[A](expr: Expr[A])(using Quotes): Option[Eval[A]] =
    import quotes.reflect.*

    val eval: Option[Eval[?]] = expr.asInstanceOf[Expr[Any]] match

      // Basic Types
      case Unseal(Literal(constant))                  => Some(Eval.Value(constant.value))
      case '{ BigInt(${ Expr(string) }: String) }     => Some(Eval.Value(BigInt(string)))
      case '{ BigInt(${ Expr(int) }: Int) }           => Some(Eval.Value(BigInt(int)))
      case '{ BigInt(${ Expr(long) }: Long) }         => Some(Eval.Value(BigInt(long)))
      case '{ BigDecimal(${ Expr(string) }: String) } => Some(Eval.Value(BigDecimal(string)))
      case '{ BigDecimal(${ Expr(int) }: Int) }       => Some(Eval.Value(BigDecimal(int)))
      case '{ BigDecimal(${ Expr(long) }: Long) }     => Some(Eval.Value(BigDecimal(long)))
      case '{ BigDecimal(${ Expr(double) }: Double) } => Some(Eval.Value(BigDecimal(double)))
      case '{ (${ Expr(string) }: String).r }         => Some(Eval.Value(string.r))

      // Implicit Conversions
      case '{ int2Integer(${ Eval(int) }: Int) } => Some(int)

      // CONTAINER TYPES
      case '{ ${ CustomFromExpr.fromExprSet(set) }: Set[a] }    => Some(Eval.Value(set))
      case '{ ${ CustomFromExpr.fromExprList(list) }: List[a] } => Some(Eval.Value(list))

      case Unseal(Ident(name)) => Some(Eval.Reference(name))

      // Boolean Operations
      case '{ (${ Eval(lhs) }: Boolean) && (${ Eval(rhs) }: Boolean) } =>
        Some(Eval.Apply1(lhs, rhs, _ && _, infix("&&")))
      case '{ (${ Eval(lhs) }: Boolean) || (${ Eval(rhs) }: Boolean) } =>
        Some(Eval.Apply1(lhs, rhs, _ || _, infix("||")))
      case '{ !(${ Eval(eval) }: Boolean) } =>
        Some(Eval.Apply0(eval, !_, prefix("!")))

      // Numeric Operations
      case '{ (${ Eval(num) }: Int).toDouble } =>
        Some(Eval.Apply0(num, _.toDouble, nullary("toDouble")))

      // asInstanceOf
      case '{ (${ Eval(eval) }: a).asInstanceOf[b] } =>
        Some(Eval.Apply0(eval, _.asInstanceOf[b], nullary(s"asInstanceOf[${Type.show[b]}]")))

      // isInstanceOf
      case '{ (${ Eval(eval) }: a).isInstanceOf[b] } =>
        Some(Eval.Apply0(eval, _.isInstanceOf[b], nullary(s"isInstanceOf${Type.show[b]}")))

      // Char Operations
      case '{ (${ Eval(char) }: Char).isDigit } =>
        Some(Eval.Apply0(char, _.isDigit, nullary("isDigit")))
      case '{ (${ Eval(char) }: Char).isLetter } =>
        Some(Eval.Apply0(char, _.isLetter, nullary("isLetter")))
      case '{ (${ Eval(char) }: Char).isLetterOrDigit } =>
        Some(Eval.Apply0(char, _.isLetterOrDigit, nullary("isLetterOrDigit")))
      case '{ (${ Eval(char) }: Char).isLower } =>
        Some(Eval.Apply0(char, _.isLower, nullary("isLower")))
      case '{ (${ Eval(char) }: Char).isUpper } =>
        Some(Eval.Apply0(char, _.isUpper, nullary("isUpper")))
      case '{ (${ Eval(char) }: Char).isWhitespace } =>
        Some(Eval.Apply0(char, _.isWhitespace, nullary("isWhitespace")))
      case '{ (${ Eval(char) }: Char).toLower } =>
        Some(Eval.Apply0(char, _.toLower, nullary("toLower")))
      case '{ (${ Eval(char) }: Char).toUpper } =>
        Some(Eval.Apply0(char, _.toUpper, nullary("toUpper")))
      case '{ (${ Eval(char) }: Char).asDigit } =>
        Some(Eval.Apply0(char, _.asDigit, nullary("asDigit")))

      // String Operations
      case '{ (${ Eval(string) }: String).length } =>
        Some(Eval.Apply0(string, _.length, nullary("length")))
      case '{ (${ Eval(string) }: String).substring(${ Eval(start) }: Int, ${ Eval(end) }: Int) } =>
        Some(Eval.Apply2(string, start, end, _.substring(_, _), call2("substring")))
      case '{ (${ Eval(string) }: String).toUpperCase } =>
        Some(Eval.Apply0(string, _.toUpperCase, nullary("toUpperCase")))
      case '{ (${ Eval(string) }: String).distinct } =>
        Some(Eval.Apply0(string, _.distinct, nullary("distinct")))
      case '{ (${ Eval(string) }: String).filter(${ Eval(predicate) }: Char => Boolean) } =>
        Some(Eval.Apply1(string, predicate, _.filter(_), call("filter")))
      case '{ (${ Eval(string) }: String).toLowerCase } =>
        Some(Eval.Apply0(string, _.toLowerCase, nullary("toLowerCase")))
      case '{ (${ Eval(str) }: String).startsWith(${ Eval(prefix) }: String) } =>
        Some(Eval.Apply1(str, prefix, _.startsWith(_), call("startsWith")))
      case '{ (${ Eval(str) }: String).endsWith(${ Eval(suffix) }: String) } =>
        Some(Eval.Apply1(str, suffix, _.endsWith(_), call("endsWith")))
      case '{ (${ Eval(str) }: String).contains(${ Eval(substr) }: String) } =>
        Some(Eval.Apply1(str, substr, _.contains(_), call("contains")))
      case '{ (${ Eval(str) }: String).matches(${ Eval(regex) }: String) } =>
        Some(Eval.Apply1(str, regex, _.matches(_), call("matches")))
      case '{ (${ Eval(regex) }: Regex).matches(${ Eval(str) }: String) } =>
        Some(Eval.Apply1(regex, str, _.matches(_), call("matches")))
      case '{ (${ Eval(str) }: String).nonEmpty } =>
        Some(Eval.Apply0(str, _.nonEmpty, nullary("nonEmpty")))
      case '{ (${ Eval(str) }: String).isEmpty } =>
        Some(Eval.Apply0(str, _.isEmpty, nullary("isEmpty")))
      case '{ (${ Eval(str) }: String).reverse } =>
        Some(Eval.Apply0(str, _.reverse, nullary("reverse")))
      case '{ (${ Eval(str) }: String)(${ Eval(index) }: Int) } =>
        Some(Eval.Apply1(str, index, _.apply(_), (l, r) => s"$l($r)"))
      case '{ (${ Eval(str) }: String).trim } =>
        Some(Eval.Apply0(str, _.trim, nullary("trim")))
      case '{ (${ Eval(str) }: String) ++ (${ Eval(other) }: String) } =>
        Some(Eval.Apply1(str, other, _ ++ _, infix("++")))
      case '{ (${ Eval(str) }: String).forall(${ Eval(predicate) }: Char => Boolean) } =>
        Some(Eval.Apply1(str, predicate, _.forall(_), call("forall")))
      case '{ (${ Eval(str) }: String).exists(${ Eval(predicate) }: Char => Boolean) } =>
        Some(Eval.Apply1(str, predicate, _.exists(_), call("exists")))
      case '{ (${ Eval(str) }: String).contains(${ Eval(char) }: Char) } =>
        Some(Eval.Apply1(str, char, _.contains(_), call("contains")))
      case '{ (${ Eval(str) }: String).toSet } =>
        Some(Eval.Apply0(str, _.toSet, nullary("toSet")))
      case '{ (${ Eval(str) }: String).stripMargin } =>
        Some(Eval.Apply0(str, _.stripMargin, nullary("stripMargin")))
      case '{ StringContext(${ Varargs(Exprs[String](strings)) }*).s(${ Varargs(Evals(evals)) }*) } =>
        Some(Eval.EvalStringContext(strings.toList, evals.toList))

      case '{ scala.Predef.identity(${ Eval(eval) }) } =>
        Some(eval)
      case MatchBinEq(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, _ == _, infix("==")))
      case MatchBinNeq(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, _ != _, infix("!=")))
      case MatchBinJavaEq(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, (a, b) => a.asInstanceOf[Object] eq b.asInstanceOf[Object], infix("eq")))
      case MatchBinJavaNeq(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, (a, b) => a.asInstanceOf[Object] ne b.asInstanceOf[Object], infix("ne")))
      case MatchBinLt(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, Operations.lessThan, infix("<")))
      case MatchBinGt(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, Operations.greaterThan, infix(">")))
      case MatchBinLte(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, Operations.lessThanOrEqual, infix("<=")))
      case MatchBinGte(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, Operations.greaterThanOrEqual, infix(">=")))
      case MatchBinMinus(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, Operations.minus, infix("-")))
      case MatchBinPlus(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, Operations.plus, infix("+")))
      case MatchBinDivide(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, Operations.divide, infix("/")))
      case MatchBinMod(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, Operations.mod, infix("%")))
      case MatchBinTimes(lhs, rhs) =>
        Some(Eval.Apply1(lhs, rhs, Operations.times, infix("*")))

      case Unseal(quotes.reflect.Block(stats, Seal(Eval(expr)))) =>
        val defs = stats.collect {
          case ValDef(name, _, Some(Seal(Eval(eval)))) =>
            EvalDef.EvalValDef(name, eval)
          case DefDef(name, valDefs, _, Some(Seal(Eval(rhs)))) =>
            val params = valDefs.flatMap(_.params).collect { case ValDef(name, _, _) => name }
            EvalDef.EvalDefDef(name, params, rhs)
        }
        Some(Eval.EvalBlock(defs, expr))

      // Match Expressions
      case Unseal(Match(Seal(Eval(expr)), caseDefs)) =>
        val calcCaseDefs = caseDefs.map(EvalMatchCase.parse)
        Some(MatchExpr(expr, calcCaseDefs))

      case '{
            if ${ Eval(cond) } then ${ Eval(trueCase) }
            else ${ Eval(falseCase) }
          } =>
        Some(Eval.IfThenElse(cond, trueCase, falseCase))

      // Remove Typed Nodes
      case Unseal(Typed(t, _)) =>
        unapply(t.asExpr)

      // Remove Inlined Nodes
      case Unseal(Uninlined(t)) =>
        unapply(t.asExpr)

      // Set Operations (+ and - are handled by the MatchBinPlus and MatchBinMinus cases)
      case '{ type a; (${ Eval(set) }: Set[`a`]).contains(${ Eval(elem) }: `a`) } =>
        Some(Eval.Apply1(set, elem, _.contains(_), call("contains")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).exists(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(set, predicate, _.exists(_), call("exists")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).forall(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(set, predicate, _.forall(_), call("forall")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).intersect(${ Eval(other) }: Set[`a`]) } =>
        Some(Eval.Apply1(set, other, _.intersect(_), call("intersect")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).union(${ Eval(other) }: Set[`a`]) } =>
        Some(Eval.Apply1(set, other, _.union(_), call("union")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).diff(${ Eval(other) }: Set[`a`]) } =>
        Some(Eval.Apply1(set, other, _.diff(_), call("diff")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).++(${ Eval(other) }: Set[`a`]) } =>
        Some(Eval.Apply1(set, other, _.++(_), infix("++")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).--(${ Eval(other) }: Set[`a`]) } =>
        Some(Eval.Apply1(set, other, _.--(_), infix("--")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).isEmpty } =>
        Some(Eval.Apply0(set, _.isEmpty, nullary("isEmpty")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).subsetOf(${ Eval(other) }: Set[`a`]) } =>
        Some(Eval.Apply1(set, other, _.subsetOf(_), call("subsetOf")))
      case '{ type a; (${ Eval(set) }: Set[`a`]).apply(${ Eval(elem) }: `a`) } =>
        Some(Eval.Apply1(set, elem, _.apply(_), (a, b) => s"$a($b)"))

      // List Operations
      case '{ type a; (${ Eval(list) }: List[`a`]).:+(${ Eval(elem) }: `a`) } =>
        Some(Eval.Apply1(list, elem, _ :+ _, infix(":+")))
      case '{ type a; (${ Eval(list) }: List[`a`]).::[`a`](${ Eval(elem) }: `a`) } =>
        Some(Eval.Apply1(elem, list, _ :: _, infix("::")))
      case '{ type a; (${ Eval(list) }: List[`a`]).:::[`a`](${ Eval(other) }: List[`a`]) } =>
        Some(Eval.Apply1(other, list, _ ::: _, infix(":::")))
      case '{ type a; (${ Eval(list) }: List[`a`]).head } =>
        Some(Eval.Apply0(list, _.head, nullary("head")))
      case '{ type a; (${ Eval(list) }: List[`a`]).tail } =>
        Some(Eval.Apply0(list, _.tail, nullary("tail")))
      case '{ type a; (${ Eval(list) }: List[`a`]).isEmpty } =>
        Some(Eval.Apply0(list, _.isEmpty, nullary("isEmpty")))
      case '{ type a; (${ Eval(list) }: List[`a`]).nonEmpty } =>
        Some(Eval.Apply0(list, _.nonEmpty, nullary("nonEmpty")))
      case '{ type a; (${ Eval(list) }: List[`a`]).size } =>
        Some(Eval.Apply0(list, _.size, nullary("size")))
      case '{ type a; (${ Eval(list) }: List[`a`]).length } =>
        Some(Eval.Apply0(list, _.length, nullary("length")))
      case '{ type a; (${ Eval(list) }: List[`a`]).contains(${ Eval(elem) }: `a`) } =>
        Some(Eval.Apply1(list, elem, _.contains(_), call("contains")))
      case '{ type a; (${ Eval(list) }: List[`a`]).exists(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.exists(_), call("exists")))
      case '{ type a; (${ Eval(list) }: List[`a`]).forall(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.forall(_), call("forall")))
      case '{ type a; (${ Eval(list) }: List[`a`]).apply(${ Eval(index) }: Int) } =>
        Some(Eval.Apply1(list, index, _.apply(_), infix("apply")))
      case '{ type a; (${ Eval(list) }: List[`a`]).filter(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.filter(_), call("filter")))
      case '{ type a; type b; (${ Eval(list) }: List[`a`]).map(${ Eval(f) }: `a` => `b`) } =>
        Some(Eval.Apply1(list, f, _.map(_), call("map")))

      // Seq Operations
      case '{ type a; (${ Eval(list) }: Seq[`a`]).forall(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.forall(_), call("forall")))
      case '{ type a; (${ Eval(list) }: Seq[`a`]).contains(${ Eval(elem) }: `a`) } =>
        Some(Eval.Apply1(list, elem, _.contains(_), call("contains")))
      case '{ type a; (${ Eval(list) }: Seq[`a`]).isEmpty } =>
        Some(Eval.Apply0(list, _.isEmpty, nullary("isEmpty")))
      case '{ type a; (${ Eval(list) }: Seq[`a`]).filterNot(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.filterNot(_), call("filterNot")))

      // Iterable Operations
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).mkString } =>
        Some(Eval.Apply0(list, _.mkString, nullary("mkString")))
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).mkString(${ Eval(string) }: String) } =>
        Some(Eval.Apply1(list, string, _.mkString(_), call("mkString")))
      case '{ type a; (${ Eval(list) }: Seq[`a`]).toSet } =>
        Some(Eval.Apply0(list, _.toSet, nullary("toSet")))

      // Range Operations
      case '{ (${ Eval(char) }: Char).to(${ Eval(end) }: Char) } =>
        Some(Eval.Apply1(char, end, _.to(_), call("to")))

      case Unseal(Closure(Ident(name), _)) =>
        Some(Eval.Reference(name))

      case Unseal(Apply(Ident(name), List(Seal(Eval(arg))))) =>
        Some(Eval.EvalApply(Eval.Reference(name), List(arg)))

      case '{ type a; type b; (${ Eval(f) }: `a` => `b`)(${ Eval(a) }: `a`) } =>
        Some(Eval.EvalApply(f, List(a)))

      case other =>
        // DEV MODE
        // report.errorAndAbort(
        //   s"""Eval parse failure:
        //      |show: ${other.show}
        //      | tpe: ${other.asTerm.tpe.show}
        //      |term: ${other.asTerm}
        //      |""".stripMargin
        // )
        None

    eval.asInstanceOf[Option[Eval[A]]]

enum EvalDef[A]:
  case EvalValDef(name: String, eval: Eval[A])
  case EvalDefDef(name: String, params: List[String], eval: Eval[A])

case class EvalMatchCase[A](pattern: EvalPattern[A], guard: Option[Eval[Boolean]], eval: Eval[A]):
  def render(using Map[String, String])(using Quotes): String =
    s"${pattern.render} => ${eval.render}"

  def matches(using ctx: Map[String, Any], q: Quotes)(value: Any): Boolean =
    val patternMatches = pattern.matches(value)
    lazy val guardResult = guard
      .map { eval =>
        val newMap = ctx ++ pattern.bindings.map(_ -> value)
        eval.result(using newMap)
      }
      .getOrElse(true)
    patternMatches && guardResult

object EvalMatchCase:
  def parse(using Quotes)(caseDef: quotes.reflect.CaseDef): EvalMatchCase[Any] =
    import quotes.reflect.*
    caseDef match
      case CaseDef(pattern, guard, Seal(Eval(eval))) =>
        val guardEval = guard.map { case Seal(Eval[Boolean](guardEval)) => guardEval }
        EvalMatchCase(EvalPattern.parse(pattern), guardEval, eval)
      case other =>
        report.errorAndAbort(s"EvalMatchCase parse failed to parse: ${other}")
        ???

enum EvalPattern[A]:
  case Value(value: Eval[A])
  case Variable(name: String)
  case Alternative(patterns: List[EvalPattern[A]])
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
      case Value(patternValue) =>
        patternValue.result == value
      case Variable(_) => true
      case Wildcard()  => true
      case Alternative(patterns) =>
        patterns.exists(_.matches(value))

object EvalPattern:
  def parse(using Quotes)(term: quotes.reflect.Tree): EvalPattern[Any] =
    import quotes.reflect as r
    term match
      case r.Wildcard()               => EvalPattern.Wildcard()
      case r.Bind(name, r.Wildcard()) => EvalPattern.Variable(name)
      case Seal(Eval(value))          => EvalPattern.Value(value)
      case r.Alternatives(patterns)   => EvalPattern.Alternative(patterns.map(parse))
      case other                      => r.report.errorAndAbort(s"EvalPattern parse failed to parse: ${other}")

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
    Some(term.asExpr)

object Evals:
  def unapply(exprs: Seq[Expr[?]])(using Quotes): Option[Seq[Eval[?]]] =
    val builder = Seq.newBuilder[Eval[?]]
    val iter    = exprs.iterator
    while iter.hasNext do
      val expr = iter.next()
      Eval.unapply(expr) match
        case Some(eval) => builder += eval
        case None       => return None
    Some(builder.result())

end Evals
