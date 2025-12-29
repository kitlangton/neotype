package comptime

import scala.quoted.*

/** Helpers for working with ToExpr in macros. */
object MacroExprsHelpers:
  /** Cast a ToExpr[T] to ToExpr[A] after a type pattern match.
    *
    * This is safe when used immediately after matching `case '[T] =>` because
    * the pattern match proves `A =:= T`, but Scala's type system can't
    * propagate that evidence through the ToExpr type parameter.
    *
    * Example:
    * {{{
    * Type.of[A] match
    *   case '[Int] => castToExpr[Int, A](summon[ToExpr[Int]])  // safe: A =:= Int proven
    * }}}
    */
  inline def castToExpr[T, A](te: ToExpr[T]): ToExpr[A] =
    te.asInstanceOf[ToExpr[A]]

  /** Option variant of castToExpr. */
  inline def castToExprOpt[T, A](opt: Option[ToExpr[T]]): Option[ToExpr[A]] =
    opt.asInstanceOf[Option[ToExpr[A]]]

object MacroExprs:
  export MacroExprsHelpers.{castToExpr, castToExprOpt}

  def summonExprOpt[A: Type](using Quotes): Option[ToExpr[A]] =
    MacroExprsPrimitive
      .summonExprOpt[A]
      .orElse(MacroExprsJavaTime.summonExprOpt[A]) // Before Enum to handle Java enums (Month, DayOfWeek)
      .orElse(MacroExprsEnum.summonExprOpt[A])
      .orElse(MacroExprsOptionEither.summonExprOpt[A])
      .orElse(MacroExprsCollections.summonExprOpt[A])
      .orElse(MacroExprsCaseClass.summonExprOpt[A])
