package comptime

import scala.quoted.*

/** Handles lifting union types (A | B) by checking which branch the runtime
  * value belongs to.
  */
private[comptime] object MacroExprsUnion:
  def summonExprOpt[A: Type](using Quotes): Option[ToExpr[A]] =
    import quotes.reflect.*

    TypeRepr.of[A] match
      case OrType(left, right) =>
        // Get ToExpr for both branches
        val leftOpt = left.asType match
          case '[l] => MacroExprs.summonExprOpt[l]
        val rightOpt = right.asType match
          case '[r] => MacroExprs.summonExprOpt[r]

        // Need at least one branch to work
        (leftOpt, rightOpt) match
          case (Some(teL), Some(teR)) =>
            Some(
              new ToExpr[A]:
                def apply(value: A)(using Quotes): Expr[A] =
                  // Try left branch first, fall back to right
                  try teL.asInstanceOf[ToExpr[Any]](value)(using summon[Quotes]).asExprOf[A]
                  catch
                    case _: ClassCastException | _: MatchError =>
                      teR.asInstanceOf[ToExpr[Any]](value)(using summon[Quotes]).asExprOf[A]
            )
          case (Some(te), None) =>
            Some(
              new ToExpr[A]:
                def apply(value: A)(using Quotes): Expr[A] =
                  te.asInstanceOf[ToExpr[Any]](value)(using summon[Quotes]).asExprOf[A]
            )
          case (None, Some(te)) =>
            Some(
              new ToExpr[A]:
                def apply(value: A)(using Quotes): Expr[A] =
                  te.asInstanceOf[ToExpr[Any]](value)(using summon[Quotes]).asExprOf[A]
            )
          case (None, None) => None

      case _ => None
