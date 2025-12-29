package comptime

import java.time.DayOfWeek
import java.time.Duration
import java.time.LocalDate
import java.time.LocalTime
import java.time.Month
import scala.quoted.*

import MacroExprs.castToExpr

object MacroExprsJavaTime:
  // Helper for custom ToExpr definitions
  private inline def custom[T, A](te: ToExpr[T]): Option[ToExpr[A]] =
    Some(castToExpr[T, A](te))

  def summonExprOpt[A: Type](using Quotes): Option[ToExpr[A]] =
    Type.of[A] match
      case '[java.time.LocalDate] =>
        custom[LocalDate, A](
          new ToExpr[LocalDate]:
            def apply(d: LocalDate)(using Quotes): Expr[LocalDate] =
              '{ java.time.LocalDate.of(${ Expr(d.getYear) }, ${ Expr(d.getMonthValue) }, ${ Expr(d.getDayOfMonth) }) }
        )
      case '[java.time.LocalTime] =>
        custom[LocalTime, A](
          new ToExpr[LocalTime]:
            def apply(t: LocalTime)(using Quotes): Expr[LocalTime] =
              '{
                java.time.LocalTime
                  .of(${ Expr(t.getHour) }, ${ Expr(t.getMinute) }, ${ Expr(t.getSecond) }, ${ Expr(t.getNano) })
              }
        )
      case '[java.time.Duration] =>
        custom[Duration, A](
          new ToExpr[Duration]:
            def apply(d: Duration)(using Quotes): Expr[Duration] =
              '{ java.time.Duration.ofSeconds(${ Expr(d.getSeconds) }, ${ Expr(d.getNano.toLong) }) }
        )
      case '[java.time.Month] =>
        custom[Month, A](
          new ToExpr[Month]:
            def apply(m: Month)(using Quotes): Expr[Month] =
              '{ java.time.Month.of(${ Expr(m.getValue) }) }
        )
      case '[java.time.DayOfWeek] =>
        custom[DayOfWeek, A](
          new ToExpr[DayOfWeek]:
            def apply(d: DayOfWeek)(using Quotes): Expr[DayOfWeek] =
              '{ java.time.DayOfWeek.of(${ Expr(d.getValue) }) }
        )
      case _ => None
