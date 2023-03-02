package neotype

import scala.annotation.tailrec
import StringFormatting.*
import scala.quoted.*
import scala.util.matching.Regex

enum Calc[A]:
  case Constant(value: A)

  // Comparisons
  case EqualTo[A, B](lhs: Calc[A], rhs: Calc[B])                                          extends Calc[Boolean]
  case GreaterThan[A](lhs: Calc[A], rhs: Calc[A])(using val ordering: Ordering[A])        extends Calc[Boolean]
  case LessThan[A](lhs: Calc[A], rhs: Calc[A])(using val ordering: Ordering[A])           extends Calc[Boolean]
  case GreaterThanOrEqual[A](lhs: Calc[A], rhs: Calc[A])(using val ordering: Ordering[A]) extends Calc[Boolean]
  case LessThanOrEqual[A](lhs: Calc[A], rhs: Calc[A])(using val ordering: Ordering[A])    extends Calc[Boolean]

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

  // Custom
  case WithMessage(calc: Calc[A], message: String)  extends Calc[A]
  case Block(defs: List[CalcDef[?]], calc: Calc[A]) extends Calc[A]
  case Reference[A](name: String)                   extends Calc[A]

  def renderConstant(value: Any): String =
    value match
      case s: String                                => s""""$s"""".green
      case c: Char                                  => s"'$c'".green
      case _: Int | Long | Float | Double | Boolean => s"$value".cyan
      case regex: Regex =>
        s"""${renderConstant(regex.toString)}.r"""
      case _ => value.toString

  def render(using ctx: Map[String, String]): String =
    this match
      case Constant(value) => renderConstant(value)

      // Comparisons
      case GreaterThan(lhs, rhs)        => s"${lhs.render} > ${rhs.render}"
      case LessThan(lhs, rhs)           => s"${lhs.render} < ${rhs.render}"
      case EqualTo(lhs, rhs)            => s"${lhs.render} == ${rhs.render}"
      case GreaterThanOrEqual(lhs, rhs) => s"${lhs.render} >= ${rhs.render}"
      case LessThanOrEqual(lhs, rhs)    => s"${lhs.render} <= ${rhs.render}"

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

      case WithMessage(calc, message) => s"${calc.render} // $message"
      case Block(defs, calc) =>
        val newCtx = defs.foldLeft(ctx) { (ctx, defn) =>
          ctx + (defn.name -> defn.calc.render(using ctx))
        }
        calc.render(using newCtx)
      case Reference("INPUT_SENTINEL") => "input".blue
      case Reference(name)             => ctx(name)

  def result(using context: Map[String, Any]): A =
    this match
      case Constant(value) => value

      // Comparisons
      case EqualTo(lhs, rhs) => (lhs.result == rhs.result).asInstanceOf[A]
      case gt @ GreaterThan(lhs, rhs): GreaterThan[num] =>
        import gt.ordering
        summon[Ordering[num]].gt(lhs.result, rhs.result).asInstanceOf[A]
      case lt @ LessThan(lhs, rhs): LessThan[num] =>
        import lt.ordering
        summon[Ordering[num]].lt(lhs.result, rhs.result).asInstanceOf[A]
      case gte @ GreaterThanOrEqual(lhs, rhs): GreaterThanOrEqual[num] =>
        import gte.ordering
        summon[Ordering[num]].gteq(lhs.result, rhs.result).asInstanceOf[A]
      case lte @ LessThanOrEqual(lhs, rhs): LessThanOrEqual[num] =>
        import lte.ordering
        summon[Ordering[num]].lteq(lhs.result, rhs.result).asInstanceOf[A]

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

      case WithMessage(calc, _) =>
        calc.result
      case Block(defs, calc) =>
        val newContext = defs.foldLeft(context) { (ctx, defn) =>
          ctx + (defn.name -> defn.calc.result)
        }
        calc.result(using newContext)
      case Reference(name) =>
        context(name).asInstanceOf[A]

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
      case Unseal(Ident(name))                        => Some(Calc.Reference(name).asInstanceOf[Calc[A]])

      // Comparisons
      case '{ (${ Calc(lhs) }: Int) == (${ Calc(rhs) }: Int) } =>
        Some(Calc.EqualTo(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Double) == (${ Calc(rhs) }: Double) } =>
        Some(Calc.EqualTo(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Char) == (${ Calc(rhs) }: Char) } =>
        Some(Calc.EqualTo(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Float) == (${ Calc(rhs) }: Float) } =>
        Some(Calc.EqualTo(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Long) == (${ Calc(rhs) }: Long) } =>
        Some(Calc.EqualTo(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Short) == (${ Calc(rhs) }: Short) } =>
        Some(Calc.EqualTo(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Byte) == (${ Calc(rhs) }: Byte) } =>
        Some(Calc.EqualTo(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Any) == (${ Calc(rhs) }: Any) } =>
        Some(Calc.EqualTo(lhs, rhs).asInstanceOf[Calc[A]])

      case '{ (${ Calc(lhs) }: Int) > (${ Calc(rhs) }: Int) } =>
        Some(Calc.GreaterThan(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Double) > (${ Calc(rhs) }: Double) } =>
        Some(Calc.GreaterThan(lhs, rhs).asInstanceOf[Calc[A]])

      // Less Than
      case '{ (${ Calc(lhs) }: Int) < (${ Calc(rhs) }: Int) } =>
        Some(Calc.LessThan(lhs, rhs).asInstanceOf[Calc[A]])
      case '{ (${ Calc(lhs) }: Double) < (${ Calc(rhs) }: Double) } =>
        Some(Calc.LessThan(lhs, rhs).asInstanceOf[Calc[A]])

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

      case Unseal(quotes.reflect.Block(stats, Seal(Calc(expr)))) =>
        val defs = stats.map { case ValDef(name, _, Some(Seal(Calc(calc)))) => CalcDef(name, calc) }
//        report.errorAndAbort(s"Defs: ${defs} stats: ${stats} expr: ${expr}")
        Some(Calc.Block(defs, expr).asInstanceOf[Calc[A]])

//      case '{ (${ Calc(calc) }: Boolean).withMessage(${ Expr(message) }: String) } =>
//        Some(Calc.WithMessage(calc, message).asInstanceOf[Calc[A]])

      case Unseal(Typed(t, _)) =>
        unapply(t.asExprOf[A])
      case Unseal(
            Apply(Apply(Apply(Ident("??"), List(_)), List(Seal(Calc(body)))), List(Literal(StringConstant(msg))))
          ) =>
        Some(Calc.WithMessage(body, msg).asInstanceOf[Calc[A]])

      // Fixing
      case other =>
        report.errorAndAbort(s"Calc unapply failed to parse: ${other.show}\n${other.asTerm.underlyingArgument}")
        None

case class CalcDef[A](name: String, calc: Calc[A])

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
