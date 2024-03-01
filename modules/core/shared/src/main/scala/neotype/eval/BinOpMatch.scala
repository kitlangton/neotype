package neotype.eval

import scala.quoted.*

case class BinOpMatch(name: String):
  def unapply(using Quotes)(expr: Expr[Any]): Option[(Eval[Any], Eval[Any])] =
    import quotes.reflect.*
    expr.asTerm match
      case Apply(Select(lhs, `name`), List(rhs)) =>
        (lhs.asExpr, rhs.asExpr) match
          case (Eval(lhs), Eval(rhs)) =>
            Some((lhs, rhs))
//          case _ =>
//            report.errorAndAbort(s"Cannot convert ${lhs.show} and ${rhs.show} to Eval")
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
