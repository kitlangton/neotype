package neotype.eval

import BinOpMatch.*

import scala.annotation.tailrec
import neotype.StringFormatting.*

import scala.quoted.*
import scala.util.matching.Regex
import CustomFromExpr.given
// import neotype.eval.Eval.closureToFunction
import java.util as ju

enum EvalError extends Throwable:
  case MissingReference(name: String) extends EvalError

  override def getMessage: String = this match
    case MissingReference(name) => s"Missing reference: $name"

sealed trait Eval[A]:
  import Eval.*

  def render(using ctx: Map[String, String])(using Quotes): String =
    import quotes.reflect.*

    this match
      case Value(value)    => Eval.renderValue(value)
      case Reference(name) => ctx.getOrElse(name, "_")

      // Comparisons
      case Apply0(eval, _, show)       => show(eval.render)
      case Apply1(a, b, _, show)       => show(a.render, b.render)
      case Apply2(a, b, c, _, show)    => show(a.render, b.render, c.render)
      case Apply3(a, b, c, d, _, show) => show(a.render, b.render, c.render, d.render)

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

      case ProductValue(name, fields) =>
        s"$name(${fields
            .map { (k, v) =>
              s"$k = ${v.render}"
            }
            .mkString(", ")})"

      case ProductSelect(eval, field) =>
        s"${eval.render}.$field"
  end render

  private def indent(str: String): String =
    str.linesIterator.map("  " + _).mkString("\n")

  def result(using context: Map[String, Any]): A =
    this match
      case Value(value) =>
        value
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
        op(eval.result, arg.result).asInstanceOf[A]

      case Apply2(eval, arg1, arg2, op, renderF) =>
        op(eval.result, arg1.result, arg2.result).asInstanceOf[A]

      case Apply3(eval, arg1, arg2, arg3, op, renderF) =>
        op(eval.result, arg1.result, arg2.result, arg3.result).asInstanceOf[A]

      case MatchExpr(expr, cases) =>
        val exprResult = expr.result
        cases.find(_.matches(exprResult)) match
          case Some(EvalMatchCase(pattern, _, eval)) =>
            val newCtx = pattern.bindings.foldLeft(context) { (ctx, name) =>
              ctx + (name -> exprResult)
            }
            eval.result(using newCtx)
          case None =>
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

      case EvalConstruct(constructor, args) =>
        constructor(args.map(_.result))

      case ProductValue(name, fields) =>
        fields.view.mapValues(_.result).toMap

      case ProductSelect(eval, field) =>
        println(s"SELECTING $field FROM $eval")
        println(s"RESULT: ${eval.result}")
        eval.result.asInstanceOf[Map[String, Any]].getOrElse(field, throw MatchError(field))

      case EvalApply(eval, args) =>
        val calcResult = eval.result
        calcResult match
          // case EvalClosure(cctx, params, body) =>
          //   val newCtx = params.zip(args.map(_.result)).foldLeft(cctx) { (ctx, arg) =>
          //     ctx + arg
          //   }
          //   body.result(using newCtx)
          case f1: Function1[Any, Any] =>
            f1.apply(args(0).result)
          case f2: Function2[Any, Any, Any] =>
            val res = f2.apply(args(0).result, args(1).result)
            res
          case _ =>
            throw MatchError(calcResult)
  end result

end Eval

object Eval:
  case class Value[A](value: A) extends Eval[A]

  // The Apply* variants represent method calls.
  // Apply0 encodes a nullary method. E.g., `string.length`
  // Apply1 encodes a unary method. E.g., `string.startsWith("a")`
  // Apply2 encodes a binary method. E.g., `string.substring(1, 2)`
  case class Apply0[A, B](eval: Eval[A], op: A => B, show: String => String) extends Eval[B]
  case class Apply1[A, B, C](eval: Eval[A], rhs: Eval[B], op: (A, B) => C, show: (String, String) => String)
      extends Eval[C]
  case class Apply2[A, B, C, D](
      eval: Eval[A],
      lhs: Eval[B],
      rhs: Eval[C],
      op: (A, B, C) => D,
      show: (String, String, String) => String
  ) extends Eval[D]
  case class Apply3[A, B, C, D, E](
      eval: Eval[A],
      arg1: Eval[B],
      arg2: Eval[C],
      arg3: Eval[D],
      op: (A, B, C, D) => E,
      show: (String, String, String, String) => String
  ) extends Eval[E]

  case class EvalConstruct[A](constructor: List[Any] => A, args: List[Eval[?]]) extends Eval[A]
  case class EvalApply[A](eval: Eval[A], args: List[Eval[?]])                   extends Eval[Any]

  case class ProductValue(name: String, fields: Map[String, Eval[?]]) extends Eval[Any]
  case class ProductSelect[A](eval: Eval[A], field: String)           extends Eval[Any]

  case class EvalStringContext[A](parts: List[String], args: List[Eval[?]]) extends Eval[String]

  case class EvalBlock[A](defs: List[EvalDef[?]], eval: Eval[A])                      extends Eval[A]
  case class Reference[A](name: String)                                               extends Eval[A]
  case class MatchExpr[A](expr: Eval[A], cases: List[EvalMatchCase[A]])               extends Eval[A]
  case class IfThenElse[A](cond: Eval[Boolean], thenEval: Eval[A], elseEval: Eval[A]) extends Eval[A]

  // case class EvalClosure[A](ctx: Map[String, Any], params: List[String], body: Eval[A]) extends Eval[A]
  object EvalClosure:
    def apply[A](ctx: Map[String, Any], params: List[String], body: Eval[A]): Eval[A] =
      params match
        case List(p1)     => EvalClosure1(ctx, p1, body)
        case List(p1, p2) => EvalClosure2(ctx, p1, p2, body)
        case _            => throw new Exception(s"Unsupported number of parameters: ${params.size}")

  case class EvalClosure1[A](ctx: Map[String, Any], p1: String, body: Eval[A]) extends Eval[A] with Function1[Any, Any]:
    def apply(a: Any): Any =
      body.result(using ctx ++ Map(p1 -> a))

  case class EvalClosure2[A](ctx: Map[String, Any], p1: String, p2: String, body: Eval[A])
      extends Eval[A]
      with Function2[Any, Any, Any]:
    def apply(a: Any, b: Any): Any =
      body.result(using ctx ++ Map(p1 -> a, p2 -> b))

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
  def call3(op: String)   = (a: String, b: String, c: String, d: String) => s"$a.$op($b, $c, $d)"
  def nullary(op: String) = (a: String) => s"$a.$op"
  def prefix(op: String)  = (a: String) => s"$op$a"

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

      case Unseal(p @ Apply(Select(_, "apply"), Evals(args))) if p.tpe.typeSymbol.flags.is(Flags.Case) =>
        val typeName   = p.tpe.typeSymbol.name
        val fieldNames = p.tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map(_.name)
        val fields     = fieldNames.zip(args).toMap
        val product    = ProductValue(typeName, fields)
        Some(product)

      case Unseal(Select(p @ Seal(Eval(eval)), field)) if p.tpe.typeSymbol.flags.is(Flags.Case) =>
        val productSelect = ProductSelect(eval, field)
        Some(productSelect)

      // Implicit Conversions
      case '{ int2Integer(${ Eval(int) }: Int) } => Some(int)

      // EMPTY
      case '{ type a; Set.empty[`a`] }              => Some(Eval.Value(Set.empty[`a`]))
      case '{ type a; List.empty[`a`] }             => Some(Eval.Value(List.empty[`a`]))
      case '{ Nil }                                 => Some(Eval.Value(Nil))
      case '{ type a; Vector.empty[`a`] }           => Some(Eval.Value(Vector.empty[`a`]))
      case '{ type a; Option.empty[`a`] }           => Some(Eval.Value(Option.empty[`a`]))
      case '{ None }                                => Some(Eval.Value(None))
      case '{ type a; type b; Map.empty[`a`, `b`] } => Some(Eval.Value(Map.empty[`a`, `b`]))

      // CONTAINER TYPES
      case '{ type a; ${ CustomFromExpr.fromExprSet(set) }: Set[`a`] }          => Some(Eval.Value(set))
      case '{ type a; ${ CustomFromExpr.fromExprList(list) }: List[`a`] }       => Some(Eval.Value(list))
      case '{ type a; ${ CustomFromExpr.fromExprVector(vector) }: Vector[`a`] } => Some(Eval.Value(vector))
      case '{ type a; ${ CustomFromExpr.fromExprOption(option) }: Option[`a`] } => Some(Eval.Value(option))

      // CONSTRUCTORS
      case '{ type a; List(${ Eval(elem) }) } =>
        Some(Eval.EvalConstruct(args => List(args*), List(elem)))
      case '{ type a; Set(${ Eval(elem) }) } =>
        Some(Eval.EvalConstruct(args => Set(args*), List(elem)))
      case '{ type a; Vector(${ Eval(elem) }) } =>
        Some(Eval.EvalConstruct(args => Vector(args*), List(elem)))

      case '{ type a; List(${ Varargs(Evals(elems)) }*) } =>
        Some(Eval.EvalConstruct(args => List(args*), elems.toList))
      case '{ type a; Set(${ Varargs(Evals(elems)) }*) } =>
        Some(Eval.EvalConstruct(args => Set(args*), elems.toList))
      case '{ type a; Vector(${ Varargs(Evals(elems)) }*) } =>
        Some(Eval.EvalConstruct(args => Vector(args*), elems.toList))
      case '{ type a; Some(${ Eval(elem) }: `a`) } =>
        Some(Eval.EvalConstruct(args => Some(args.head), List(elem)))
      case '{ type a; Option(${ Eval(elem) }: `a`) } =>
        Some(Eval.EvalConstruct(args => Option(args.head), List(elem)))

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

      // case Unseal(Lambda(args, Seal(Eval(body)))) =>
      //   val params = args.map(_.name)
      //   Some(
      //     EvalBlock(
      //       List(EvalDef.EvalDefDef("$anonfunc", params, body)),
      //       Eval.Reference("$anonfunc")
      //     )
      //   )

      case Unseal(quotes.reflect.Block(stats, rhs @ Seal(Eval(exprEval)))) =>
        val defs = stats.collect {
          case ValDef(name, _, Some(Seal(Eval(eval)))) =>
            EvalDef.EvalValDef(name, eval)
          case DefDef(name, valDefs, _, Some(Seal(Eval(rhs)))) =>
            val params = valDefs.flatMap(_.params).collect { case ValDef(name, _, _) => name }
            EvalDef.EvalDefDef(name, params, rhs)
        }
        Some(Eval.EvalBlock(defs, exprEval))

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
      case '{ type a; (${ Eval(list) }: Seq[`a`]).head } =>
        Some(Eval.Apply0(list, _.head, nullary("head")))
      case '{ type a; (${ Eval(list) }: Seq[`a`]).tail } =>
        Some(Eval.Apply0(list, _.tail, nullary("tail")))
      // case '{ type a; (${ Eval(list) }: Seq[`a`]).isEmpty } =>
      //   Some(Eval.Apply0(list, _.isEmpty, nullary("isEmpty")))
      // case '{ type a; (${ Eval(list) }: Seq[`a`]).nonEmpty } =>
      //   Some(Eval.Apply0(list, _.nonEmpty, nullary("nonEmpty")))
      case '{ type a; (${ Eval(list) }: List[`a`]).length } =>
        Some(Eval.Apply0(list, _.length, nullary("length")))

      case '{ type a; (${ Eval(list) }: Seq[`a`]).apply(${ Eval(index) }: Int) } =>
        Some(Eval.Apply1(list, index, _.apply(_), infix("apply")))

      // Seq Operations
      case '{ type a; (${ Eval(list) }: Seq[`a`]).forall(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.forall(_), call("forall")))
      case '{ type a; (${ Eval(list) }: Seq[`a`]).contains(${ Eval(elem) }: `a`) } =>
        Some(Eval.Apply1(list, elem, _.contains(_), call("contains")))
      case '{ type a; (${ Eval(list) }: Seq[`a`]).isEmpty } =>
        Some(Eval.Apply0(list, _.isEmpty, nullary("isEmpty")))
      case '{ type a; (${ Eval(list) }: Seq[`a`]).filterNot(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.filterNot(_), call("filterNot")))

      // Iterable Ops
      //  iterable.++(iterable)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).++(${ Eval(other) }: Iterable[`a`]) } =>
        Some(Eval.Apply1(list, other, _.++(_), infix("++")))

      //  iterable.collect { case a => a }
      // case '{ type a; type b; (${ Eval(list) }: Iterable[`a`]).collect(${ Eval(pf) }: PartialFunction[`a`, `b`]) } =>
      //   Some(Eval.Apply1(list, pf, _.collect(_), call("collect")))

      //  iterable.collectFirst { case a => a }

      //  iterable.concat(iterable)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).concat(${ Eval(other) }: Iterable[`a`]) } =>
        Some(Eval.Apply1(list, other, _.concat(_), call("concat")))

      //  iterable.count(_ => true)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).count(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.count(_), call("count")))

      //  iterable.drop(1)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).drop(${ Eval(n) }: Int) } =>
        Some(Eval.Apply1(list, n, _.drop(_), call("drop")))

      //  iterable.dropRight(1)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).dropRight(${ Eval(n) }: Int) } =>
        Some(Eval.Apply1(list, n, _.dropRight(_), call("dropRight")))

      //  iterable.dropWhile(_ => true)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).dropWhile(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.dropWhile(_), call("dropWhile")))

      //  iterable.empty
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).empty } =>
        Some(Eval.Apply0(list, _.empty, nullary("empty")))

      //  iterable.exists(_ => true)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).exists(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.exists(_), call("exists")))

      //  iterable.filter(_ => true)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).filter(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.filter(_), call("filter")))

      //  iterable.filterNot(_ => true)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).filterNot(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.filterNot(_), call("filterNot")))

      //  iterable.find(_ => true)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).find(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.find(_), call("find")))

      //  iterable.flatMap(a => List(a))
      case '{ type a; type b; (${ Eval(list) }: List[`a`]).flatMap(${ Eval(f) }: `a` => IterableOnce[`b`]) } =>
        Some(Eval.Apply1(list, f, _.flatMap(_), call("flatMap")))
      case '{ type a; type b; (${ Eval(list) }: Vector[`a`]).flatMap(${ Eval(f) }: `a` => IterableOnce[`b`]) } =>
        Some(Eval.Apply1(list, f, _.flatMap(_), call("flatMap")))
      case '{ type a; type b; (${ Eval(list) }: Set[`a`]).flatMap(${ Eval(f) }: `a` => IterableOnce[`b`]) } =>
        Some(Eval.Apply1(list, f, _.flatMap(_), call("flatMap")))

      // iterable.flatten
      // TODO: Ensure this is correct
      case '{ type a; (${ Eval(list) }: Iterable[Iterable[`a`]]).flatten } =>
        Some(Eval.Apply0(list, _.flatten, nullary("flatten")))

      //  iterable.fold(0)(_ + _)
      case '{
            type a;
            (${ Eval(list) }: Iterable[`a`]).fold[`a`](${ Eval(zero) }: `a`)(${ Eval(op) }: (`a`, `a`) => `a`)
          } =>
        Some(Eval.Apply2(list, zero, op, _.fold(_)(_), call2("fold")))

      //  iterable.foldLeft(0)(_ + _)
      case '{
            type a; type b;
            (${ Eval(as) }: scala.collection.Iterable[`a`]).foldLeft[`b`](${ Eval(zero) }: `b`)(${
              Eval(op)
            }: (`b`, `a`) => `b`)
          } =>
        Some(Eval.Apply2(as, zero, op, _.foldLeft(_)(_), call2("foldLeft")))

      //  iterable.foldRight(0)(_ + _)
      case '{
            type a; type b;
            (${ Eval(as) }: scala.collection.Iterable[`a`]).foldRight[`b`](${ Eval(zero) }: `b`)(${
              Eval(op)
            }: (`a`, `b`) => `b`)
          } =>
        Some(Eval.Apply2(as, zero, op, _.foldRight(_)(_), call2("foldRight")))

      //  iterable.forall(_ => true)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).forall(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.forall(_), call("forall")))

      //  iterable.foreach(_ => ())
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).foreach(${ Eval(f) }: `a` => Unit) } =>
        Some(Eval.Apply1(list, f, _.foreach(_), call("foreach")))

      //  iterable.groupBy(_ => 0)
      case '{ type a; type k; (${ Eval(list) }: Iterable[`a`]).groupBy(${ Eval(f) }: `a` => `k`) } =>
        Some(Eval.Apply1(list, f, _.groupBy(_), call("groupBy")))

      //  iterable.groupMap(_ => 0)(_ => 0)
      //   def groupMap[K, B](key: A => K)(f: A => B): immutable.Map[K, CC[B]] = {
      case '{
            type a; type b; type k;
            (${ Eval(list) }: Iterable[`a`]).groupMap[`k`, `b`](${ Eval(f) }: `a` => `k`)(${
              Eval(g)
            }: `a` => `b`)
          } =>
        Some(Eval.Apply2(list, f, g, _.groupMap(_)(_), call2("groupMap")))

      //  iterable.groupMapReduce(_ => 0)(_ => 0)(_ + _)
      //   def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): immutable.Map[K, B] = {
      case '{
            type a; type b; type k;
            (${ Eval(list) }: Iterable[`a`]).groupMapReduce[`k`, `b`](${ Eval(f) }: `a` => `k`)(${
              Eval(g)
            }: `a` => `b`)(${ Eval(reduce) }: (`b`, `b`) => `b`)
          } =>
        Some(Eval.Apply3(list, f, g, reduce, _.groupMapReduce(_)(_)(_), call3("groupMapReduce")))

      //  iterable.grouped(1)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).grouped(${ Eval(size) }: Int) } =>
        Some(Eval.Apply1(list, size, _.grouped(_), call("grouped")))

      //  iterable.head
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).head } =>
        Some(Eval.Apply0(list, _.head, nullary("head")))

      //  iterable.headOption
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).headOption } =>
        Some(Eval.Apply0(list, _.headOption, nullary("headOption")))

      //  iterable.init
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).init } =>
        Some(Eval.Apply0(list, _.init, nullary("init")))

      //  iterable.inits
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).inits } =>
        Some(Eval.Apply0(list, _.inits, nullary("inits")))
      //  iterable.knownSize
      //  iterable.isEmpty
      //  iterable.last
      //  iterable.lastOption
      //  iterable.map(a => a)
      //  iterable.max
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).max(using ${ MatchOrdering[`a`](ordering) }) } =>
        given Ordering[`a`] = ordering
        Some(Eval.Apply0(list, _.max, nullary("max")))
      //  iterable.maxOption
      //  iterable.maxBy(_ => 0)
      //  iterable.maxByOption(_ => 0)
      //  iterable.mkString
      //  iterable.mkString(",")
      //  iterable.mkString("[", ",", "]")
      //  iterable.min
      //  iterable.minOption
      //  iterable.minBy(_ => 0)
      //  iterable.minByOption(_ => 0)
      //  iterable.nonEmpty
      //  iterable.partition(_ => true)
      //  iterable.partitionMap(a => Right(a))
      //  iterable.reduce(_ + _)
      //  iterable.reduceOption(_ + _)
      //  iterable.reduceLeft(_ + _)
      //  iterable.reduceLeftOption(_ + _)
      //  iterable.reduceRight(_ + _)
      //  iterable.reduceRightOption(_ + _)
      //  iterable.scan(0)(_ + _)
      //  iterable.scanLeft(0)(_ + _)
      //  iterable.scanRight(0)(_ + _)
      //  iterable.size
      //  iterable.sizeCompare(0)
      //  iterable.sizeCompare(iterable)
      //  iterable.sizeIs > 5
      //  iterable.slice(0, 1)
      //  iterable.sliding(3)
      //  iterable.sliding(3, 1)
      //  iterable.span(_ => true)
      //  iterable.splitAt(1)
      //  iterable.stepper
      //  iterable.sum
      //  iterable.tail
      //  iterable.tails
      //  iterable.take(1)
      //  iterable.takeRight(1)
      //  iterable.takeWhile(_ => true)
      //  iterable.tapEach(_ => ())
      //  iterable.toArray
      //  iterable.toBuffer
      //  iterable.toIndexedSeq
      //  iterable.toList
      //  // iterable.toMap
      //  iterable.toSeq
      //  iterable.toSet
      //  iterable.toVector
      //  // iterable.transpose
      //  // iterable.unzip
      //  // iterable.unzip3
      //  iterable.view
      //  iterable.withFilter(_ => true)
      //  iterable.zip(iterable)
      //  iterable.zipAll(iterable, 0, 0)
      //  iterable.zipWithIndex

      // Iterable Operations
      // .mkString
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).mkString } =>
        Some(Eval.Apply0(list, _.mkString, nullary("mkString")))
      // .mkString(sep)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).mkString(${ Eval(sep) }: String) } =>
        Some(Eval.Apply1(list, sep, _.mkString(_), call("mkString")))
      // .toSet
      case '{ type a; (${ Eval(list) }: Seq[`a`]).toSet } =>
        Some(Eval.Apply0(list, _.toSet, nullary("toSet")))
      // .map(f)
      case '{ type a; type b; (${ Eval(list) }: Iterable[`a`]).map(${ Eval(f) }: `a` => `b`) } =>
        Some(Eval.Apply1(list, f, _.map(_), call("map")))
      // .filter(f)
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).filter(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.filter(_), call("filter")))
      // .isEmpty
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).isEmpty } =>
        Some(Eval.Apply0(list, _.isEmpty, nullary("isEmpty")))
      // .nonEmpty
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).nonEmpty } =>
        Some(Eval.Apply0(list, _.nonEmpty, nullary("nonEmpty")))
      // .contains
      // case '{ type a; (${ Eval(list) }: Iterable[`a`]).contains(${ Eval(elem) }: `a`) } =>
      //   Some(Eval.Apply1(list, elem, _.contains(_), call("contains")))

      // .exists
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).exists(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.exists(_), call("exists")))
      // .forall
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).forall(${ Eval(predicate) }: `a` => Boolean) } =>
        Some(Eval.Apply1(list, predicate, _.forall(_), call("forall")))
      // .size
      case '{ type a; (${ Eval(list) }: Iterable[`a`]).size } =>
        Some(Eval.Apply0(list, _.size, nullary("size")))
      // .length
      // case '{ type a; (${ Eval(list) }: Iterable[`a`]).length } =>
      //   Some(Eval.Apply0(list, _.size, nullary("length")))

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
        report.errorAndAbort(
          s"""Eval parse failure:
             |show: ${other.show}
             | tpe: ${other.asTerm.tpe.show}
             |term: ${other.asTerm}
             |""".stripMargin
        )
        None

    eval.asInstanceOf[Option[Eval[A]]]

enum EvalDef[A]:
  case EvalValDef(name: String, eval: Eval[A])
  case EvalDefDef(name: String, params: List[String], eval: Eval[A])

case class EvalMatchCase[A](pattern: EvalPattern[A], guard: Option[Eval[Boolean]], eval: Eval[A]):
  def render(using Map[String, String])(using Quotes): String =
    s"${pattern.render} => ${eval.render}"

  def matches(using ctx: Map[String, Any])(value: Any): Boolean =
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

  def matches(value: Any)(using Map[String, Any]): Boolean =
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

  def unapply(using Quotes)(terms: Seq[quotes.reflect.Term]): Option[Seq[Eval[?]]] =
    val builder = Seq.newBuilder[Eval[?]]
    val iter    = terms.iterator
    while iter.hasNext do
      val term = iter.next()
      Eval.unapply(term.asExpr) match
        case Some(eval) => builder += eval
        case None       => return None
    Some(builder.result())

end Evals

object MatchOrdering:
  def unapply[A](expr: Expr[Ordering[?]])(using Quotes): Option[Ordering[A]] =
    val ordering = expr match
      case '{ Ordering.Int }     => Some(summon[Ordering[Int]])
      case '{ Ordering.Long }    => Some(summon[Ordering[Long]])
      case '{ Ordering.Double }  => Some(summon[Ordering[Double]])
      case '{ Ordering.Float }   => Some(summon[Ordering[Float]])
      case '{ Ordering.Byte }    => Some(summon[Ordering[Byte]])
      case '{ Ordering.Short }   => Some(summon[Ordering[Short]])
      case '{ Ordering.Char }    => Some(summon[Ordering[Char]])
      case '{ Ordering.Boolean } => Some(summon[Ordering[Boolean]])
      case '{ Ordering.Unit }    => Some(summon[Ordering[Unit]])
      case '{ Ordering.String }  => Some(summon[Ordering[String]])
      case _                     => None
    ordering.asInstanceOf[Option[Ordering[A]]]
