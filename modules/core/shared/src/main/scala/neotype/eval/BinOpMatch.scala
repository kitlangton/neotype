package neotype.eval

import scala.quoted.*
import scala.collection.immutable.NumericRange.Inclusive

case class BinOpMatch(name: String):
  def unapply(using Quotes)(expr: Expr[Any]): Option[(Eval[Any], Eval[Any])] =
    import quotes.reflect.*
    expr.asTerm match
      case Apply(Select(lhs, `name`), List(rhs)) =>
        (lhs.asExpr, rhs.asExpr) match
          case (Eval(lhs), Eval(rhs)) =>
            Some((lhs, rhs))

          case s =>
            report.errorAndAbort(s"""
            |Cannot match BinOp 
            |name: $name 
            |lhs: ${lhs.show}
            |rhs: ${rhs.show}
            """.stripMargin)
      case _ =>
        None

object BinOpMatch:
  val MatchBinEq      = BinOpMatch("==")
  val MatchBinJavaEq  = BinOpMatch("eq")
  val MatchBinNeq     = BinOpMatch("!=")
  val MatchBinJavaNeq = BinOpMatch("neq")
  val MatchBinLt      = BinOpMatch("<")
  val MatchBinGt      = BinOpMatch(">")
  val MatchBinLte     = BinOpMatch("<=")
  val MatchBinGte     = BinOpMatch(">=")
  val MatchBinMinus   = BinOpMatch("-")
  val MatchBinPlus    = BinOpMatch("+")
  val MatchBinTimes   = BinOpMatch("*")
  val MatchBinDivide  = BinOpMatch("/")
  val MatchBinMod     = BinOpMatch("%")
