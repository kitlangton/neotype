package neotype

import scala.quoted.*

case class BinOpMatch(name: String):
  def unapply(using Quotes)(expr: Expr[Any]): Option[(Calc[Any], Calc[Any])] =
    import quotes.reflect.*
    expr.asTerm match
      case Apply(Select(lhs, `name`), List(rhs)) =>
        (lhs.asExpr, rhs.asExpr) match
          case (Calc(lhs), Calc(rhs)) =>
            Some((lhs, rhs))
//          case _ =>
//            report.errorAndAbort(s"Cannot convert ${lhs.show} and ${rhs.show} to Calc")
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
