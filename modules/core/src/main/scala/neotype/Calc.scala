package neotype

import scala.annotation.tailrec
import StringFormatting.*
import scala.quoted.*
import scala.util.matching.Regex

enum Calc[A]:
  case Constant(value: A)

  // Comparisons
  case BinaryOp[A, B](lhs: Calc[A], rhs: Calc[B], op: (A, B) => Boolean, name: String) extends Calc[Boolean]

  // Boolean Algebra
  case And(lhs: Calc[Boolean], rhs: Calc[Boolean]) extends Calc[Boolean]
  case Or(lhs: Calc[Boolean], rhs: Calc[Boolean])  extends Calc[Boolean]
  case Not(calc: Calc[Boolean])                    extends Calc[Boolean]

  // Numeric Operations
  case Add[Num](lhs: Calc[Num], rhs: Calc[Num])(using val numeric: Numeric[Num]) extends Calc[Num]
  case ToDouble[Num](num: Calc[Num])(using val numeric: Numeric[Num])            extends Calc[Double]

  // String Operations
  case Length(str: Calc[String])                                      extends Calc[Int]
  case Substring(str: Calc[String], start: Calc[Int], end: Calc[Int]) extends Calc[String]
  case ToUpper(str: Calc[String])                                     extends Calc[String]
  case ToLower(str: Calc[String])                                     extends Calc[String]
  case StartsWith(str: Calc[String], prefix: Calc[String])            extends Calc[Boolean]
  case EndsWith(str: Calc[String], suffix: Calc[String])              extends Calc[Boolean]
  case Contains(str: Calc[String], substr: Calc[String])              extends Calc[Boolean]
  case MatchesRegex(str: Calc[String], regex: Calc[String])           extends Calc[Boolean]
  case RegexMatches(regex: Calc[Regex], str: Calc[String])            extends Calc[Boolean]
  case StringNonEmpty(str: Calc[String])                              extends Calc[Boolean]
  case StringIsEmpty(str: Calc[String])                               extends Calc[Boolean]
  case StringApply(str: Calc[String], index: Calc[Int])               extends Calc[Char]
  case StringTrim(str: Calc[String])                                  extends Calc[String]

  //  Set Operations
  case SetContains[A](set: Calc[Set[A]], elem: Calc[A]) extends Calc[Boolean]

  // Custom
  case WithMessage(calc: Calc[A], message: String)  extends Calc[A]
  case Block(defs: List[CalcDef[?]], calc: Calc[A]) extends Calc[A]
  case Reference[A](name: String)                   extends Calc[A]

  case MatchExpr[A](expr: Calc[A], cases: List[CalcMatchCase[A]]) extends Calc[A]

  def renderConstant(value: Any): String =
    value match
      case s: String => s""""$s"""".green
      case c: Char   => s"'$c'".green
      case set: Set[?] =>
        val elems = set.map(renderConstant).mkString(", ".reset)
        s"Set(".reset + elems + ")".reset
      case regex: Regex =>
        s"""${renderConstant(regex.toString)}.r"""
      case _ => value.toString.cyan

  def render(using ctx: Map[String, String]): String =
    this match
      case Constant(value) => renderConstant(value)

      // Comparisons
      case BinaryOp(lhs, rhs, _, name) => s"${lhs.render} $name ${rhs.render}"

      // Boolean Operations
      case And(lhs, rhs) => s"${lhs.render} && ${rhs.render}"
      case Or(lhs, rhs)  => s"${lhs.render} || ${rhs.render}"
      case Not(calc)     => s"!${calc.render}"

      case Add(lhs, rhs) => s"${lhs.render} + ${rhs.render}"
      case ToDouble(num) => s"toDouble(${num.render})"

      // String Operations
      case Length(str)                => s"${str.render}.length"
      case Substring(str, start, end) => s"${str.render}.substring(${start.render}, ${end.render})"
      case ToUpper(str)               => s"${str.render}.toUpperCase"
      case ToLower(str)               => s"${str.render}.toLowerCase"
      case MatchesRegex(str, regex)   => s"${str.render}.matches(${regex.render})"
      case StartsWith(str, prefix)    => s"${str.render}.startsWith(${prefix.render})"
      case EndsWith(str, suffix)      => s"${str.render}.endsWith(${suffix.render})"
      case Contains(str, substr)      => s"${str.render}.contains(${substr.render})"
      case RegexMatches(regex, str)   => s"${regex.render}.matches(${str.render})"
      case StringNonEmpty(str)        => s"${str.render}.nonEmpty"
      case StringIsEmpty(str)         => s"${str.render}.isEmpty"
      case StringApply(str, index)    => s"${str.render}(${index.render})"
      case StringTrim(str)            => s"${str.render}.trim"

      // Set Operations
      case SetContains(set, elem) => s"${set.render}.contains(${elem.render})"

      case WithMessage(calc, message) => s"${calc.render} // $message"
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
      case Constant(value) => value

      // Comparisons
      case BinaryOp(lhs, rhs, op, name) =>
        val cool = op(lhs.result, rhs.result).asInstanceOf[A]
        cool

      // Boolean Operations
      case And(lhs, rhs) => (lhs.result && rhs.result).asInstanceOf[A]
      case Or(lhs, rhs)  => (lhs.result || rhs.result).asInstanceOf[A]
      case Not(calc)     => (!calc.result).asInstanceOf[A]

      case add @ Add(lhs, rhs): Add[num] =>
        import add.numeric
        summon[Numeric[num]].plus(lhs.result, rhs.result).asInstanceOf[A]
      case calc @ ToDouble(num): ToDouble[num] =>
        import calc.numeric
        summon[Numeric[num]].toDouble(num.result).asInstanceOf[A]

      // String Operations
      case Length(str)                => str.result.length
      case Substring(str, start, end) => str.result.substring(start.result, end.result)
      case ToUpper(str)               => str.result.toUpperCase
      case ToLower(str)               => str.result.toLowerCase
      case MatchesRegex(str, regex)   => str.result.matches(regex.result)
      case StartsWith(str, prefix)    => str.result.startsWith(prefix.result)
      case EndsWith(str, suffix)      => str.result.endsWith(suffix.result)
      case Contains(str, substr)      => str.result.contains(substr.result)
      case RegexMatches(regex, str)   => regex.result.matches(str.result)
      case StringNonEmpty(str)        => str.result.nonEmpty
      case StringIsEmpty(str)         => str.result.isEmpty
      case StringApply(str, index)    => str.result(index.result)
      case StringTrim(str)            => str.result.trim

      // Set Operations
      case SetContains(set, elem) => set.result.contains(elem.result)

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

      case WithMessage(calc, _) =>
        calc.result
      case Block(defs, calc) =>
        val newContext = defs.foldLeft(context) { (ctx, defn) =>
          ctx + (defn.name -> defn.calc.result)
        }
        calc.result(using newContext)
      case Reference(name) =>
        context(name).asInstanceOf[A]

case class BinaryOpMatch(name: String):
  def unapply(using Quotes)(expr: Expr[Any]): Option[(Calc[Any], Calc[Any])] =
    import quotes.reflect.*
    expr.asTerm match
      case Apply(Select(lhs, `name`), List(rhs)) =>
        (lhs.asExpr, rhs.asExpr) match
          case (Calc(lhs), Calc(rhs)) =>
            Some((lhs, rhs))
      case _ => None

val MatchBinEq  = BinaryOpMatch("==")
val MatchBinLt  = BinaryOpMatch("<")
val MatchBinGt  = BinaryOpMatch(">")
val MatchBinLte = BinaryOpMatch("<=")
val MatchBinGte = BinaryOpMatch(">=")

object Calc:
  def unapply[A: Type](expr: Expr[A])(using Quotes): Option[Calc[A]] =
    import quotes.reflect.*
    expr match
      case '{ ${ Expr(int) }: Int }                   => Some(Calc.Constant(int).asInstanceOf[Calc[A]])
      case '{ ${ Expr(string) }: String }             => Some(Calc.Constant(string).asInstanceOf[Calc[A]])
      case '{ ${ Expr(bool) }: Boolean }              => Some(Calc.Constant(bool).asInstanceOf[Calc[A]])
      case '{ ${ Expr(long) }: Long }                 => Some(Calc.Constant(long).asInstanceOf[Calc[A]])
      case '{ ${ Expr(double) }: Double }             => Some(Calc.Constant(double).asInstanceOf[Calc[A]])
      case '{ ${ Expr(float) }: Float }               => Some(Calc.Constant(float).asInstanceOf[Calc[A]])
      case '{ ${ Expr(char) }: Char }                 => Some(Calc.Constant(char).asInstanceOf[Calc[A]])
      case '{ ${ Expr(byte) }: Byte }                 => Some(Calc.Constant(byte).asInstanceOf[Calc[A]])
      case '{ ${ Expr(short) }: Short }               => Some(Calc.Constant(short).asInstanceOf[Calc[A]])
      case '{ () }                                    => Some(Calc.Constant(()).asInstanceOf[Calc[A]])
      case '{ BigInt(${ Expr(string) }: String) }     => Some(Calc.Constant(BigInt(string)).asInstanceOf[Calc[A]])
      case '{ BigDecimal(${ Expr(string) }: String) } => Some(Calc.Constant(BigDecimal(string)).asInstanceOf[Calc[A]])
      case '{ (${ Expr(string) }: String).r }         => Some(Calc.Constant(string.r).asInstanceOf[Calc[A]])
      // Set
      case '{ ${ Expr[Set[String]](set) }: Set[String] } => Some(Calc.Constant(set).asInstanceOf[Calc[A]])
      case Unseal(Ident(name))                           => Some(Calc.Reference(name).asInstanceOf[Calc[A]])

      // Boolean Operations
      case '{ (${ Calc(lhs) }: Boolean) && (${ Calc(rhs) }: Boolean) } =>
        Some(Calc.And(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Boolean) || (${ Calc(rhs) }: Boolean) } =>
        Some(Calc.Or(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ !(${ Calc(calc) }: Boolean) } =>
        Some(Calc.Not(calc).asInstanceOf[Calc[A]])

      // Numeric Operations
      case '{ (${ Calc(lhs) }: Int) + (${ Calc(rhs) }: Int) } =>
        Some(Calc.Add(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(num) }: Int).toDouble } =>
        Some(Calc.ToDouble(num).asInstanceOf[Calc[A]])

      // String Operations
      case '{ (${ Calc(string) }: String).length } =>
        Some(Calc.Length(string).asInstanceOf[Calc[A]])
      case '{ (${ Calc(string) }: String).length() } =>
        Some(Calc.Length(string).asInstanceOf[Calc[A]])
      case '{ (${ Calc(string) }: String).substring(${ Calc(start) }: Int, ${ Calc(end) }: Int) } =>
        Some(Calc.Substring(string, start, end).asInstanceOf[Calc[A]])
      case '{ (${ Calc(string) }: String).toUpperCase } =>
        Some(Calc.ToUpper(string).asInstanceOf[Calc[A]])
      case '{ (${ Calc(string) }: String).toLowerCase } =>
        Some(Calc.ToLower(string).asInstanceOf[Calc[A]])
      case '{ (${ Calc(str) }: String).startsWith(${ Calc(prefix) }: String) } =>
        Some(Calc.StartsWith(str, prefix).asInstanceOf[Calc[A]])
      case '{ (${ Calc(str) }: String).endsWith(${ Calc(suffix) }: String) } =>
        Some(Calc.EndsWith(str, suffix).asInstanceOf[Calc[A]])
      case '{ (${ Calc(str) }: String).contains(${ Calc(substr) }: String) } =>
        Some(Calc.Contains(str, substr).asInstanceOf[Calc[A]])
      case '{ (${ Calc(str) }: String).matches(${ Calc(regex) }: String) } =>
        Some(Calc.MatchesRegex(str, regex).asInstanceOf[Calc[A]])
      case '{ (${ Calc(regex) }: Regex).matches(${ Calc(str) }: String) } =>
        Some(Calc.RegexMatches(regex, str).asInstanceOf[Calc[A]])
      case '{ (${ Calc(str) }: String).nonEmpty } =>
        Some(Calc.StringNonEmpty(str).asInstanceOf[Calc[A]])
      case '{ (${ Calc(str) }: String).isEmpty } =>
        Some(Calc.StringIsEmpty(str).asInstanceOf[Calc[A]])
      case '{ (${ Calc(str) }: String).apply(${ Calc(index) }: Int) } =>
        Some(Calc.StringApply(str, index).asInstanceOf[Calc[A]])
      case '{ (${ Calc(str) }: String)(${ Calc(index) }: Int) } =>
        Some(Calc.StringApply(str, index).asInstanceOf[Calc[A]])
      case '{ (${ Calc(str) }: String).trim } =>
        Some(Calc.StringTrim(str).asInstanceOf[Calc[A]])

      // Set Operations
      case '{ (${ Calc(set) }: Set[String]).contains(${ Calc(elem) }: String) } =>
        Some(Calc.SetContains(set, elem).asInstanceOf[Calc[A]])

      case Unseal(quotes.reflect.Block(stats, Seal(Calc(expr)))) =>
        val defs = stats.collect { case ValDef(name, _, Some(Seal(Calc(calc)))) =>
          CalcDef(name, calc)
        }
//        report.errorAndAbort(s"Defs: ${defs} stats: ${stats} expr: ${expr}")
        Some(Calc.Block(defs, expr).asInstanceOf[Calc[A]])

      case MatchBinEq(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, _ == _, "==").asInstanceOf[Calc[A]])
      case MatchBinLt(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.lessThan, "<").asInstanceOf[Calc[A]])
      case MatchBinGt(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.greaterThan, ">").asInstanceOf[Calc[A]])
      case MatchBinLte(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.lessThanOrEqual, "<=").asInstanceOf[Calc[A]])
      case MatchBinGte(lhs, rhs) =>
        Some(Calc.BinaryOp(lhs, rhs, Operations.greaterThanOrEqual, ">=").asInstanceOf[Calc[A]])

      // parse match expression
      case Unseal(Match(Seal(Calc(expr)), caseDefs)) =>
        val calcCaseDefs = caseDefs.map(CalcMatchCase.parse)
//        report.errorAndAbort(s"Match: ${calcCaseDefs}")
        Some(MatchExpr(expr, calcCaseDefs).asInstanceOf[Calc[A]])

      case Unseal(Typed(t, _)) =>
        unapply(t.asExprOf[A])
      case Unseal(
            Apply(Apply(Apply(Ident("??"), List(_)), List(Seal(Calc(body)))), List(Literal(StringConstant(msg))))
          ) =>
        Some(Calc.WithMessage(body, msg).asInstanceOf[Calc[A]])

      // Fixing
      case other =>
//        report.errorAndAbort(s"Calc unapply failed to parse: ${other.show}\n${other.asTerm.underlyingArgument}")
        None

object Operations:
  // define an any that works for any combination of
  // Int < Int
  // Long < Long
  // Int < Long
  // Long < Int
  // etc
  def lessThan(lhs: Any, rhs: Any): Boolean =
    compare(lhs, rhs) < 0

  def greaterThan(lhs: Any, rhs: Any): Boolean =
    compare(lhs, rhs) > 0

  def greaterThanOrEqual(lhs: Any, rhs: Any): Boolean =
    compare(lhs, rhs) >= 0

  def lessThanOrEqual(lhs: Any, rhs: Any): Boolean =
    compare(lhs, rhs) <= 0

  def compare(lhs: Any, rhs: Any): Int =
    (lhs, rhs) match
      case (lhs: String, rhs: String) => lhs.compare(rhs)
      case _ =>
        val ln = numericFor(lhs).asInstanceOf[Numeric[Any]]
        val rn = numericFor(rhs).asInstanceOf[Numeric[Any]]
        ln.toDouble(lhs).compare(rn.toDouble(rhs))

  def numericFor(any: Any): Numeric[?] =
    any match
      case _: Int        => summon[Numeric[Int]]
      case _: Long       => summon[Numeric[Long]]
      case _: Short      => summon[Numeric[Short]]
      case _: Char       => summon[Numeric[Char]]
      case _: Byte       => summon[Numeric[Byte]]
      case _: Double     => summon[Numeric[Double]]
      case _: Float      => summon[Numeric[Float]]
      case _: BigInt     => summon[Numeric[BigInt]]
      case _: BigDecimal => summon[Numeric[BigDecimal]]
      case _             => throw new IllegalArgumentException(s"Cannot find numeric for ${any}")

case class CalcDef[A](name: String, calc: Calc[A])
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
      case CaseDef(pattern, guard, Seal(Calc(calc))) =>
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
      case Seal(Calc(constant))       => CalcPattern.Constant(constant)
      case r.Alternatives(patterns)   => CalcPattern.Alternative(patterns.map(parse))

object Unseal:
  def unapply(expr: Expr[?])(using Quotes): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    Uninlined.unapply(expr.asTerm.underlyingArgument)

object Uninlined:
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    term match
      case Inlined(_, bindings, t) => Some(quotes.reflect.Block(bindings, t))
      case t                       => Some(t)

object Seal:
  // turn term into expr
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[Expr[Any]] =
    import quotes.reflect.*
    Some(term.asExpr)
