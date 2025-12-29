package comptime

import scala.quoted.*

import MacroExprs.castToExpr

private[comptime] object MacroExprsPrimitive:
  // Helper for primitives with existing ToExpr instances
  private inline def primitive[T: ToExpr, A]: Option[ToExpr[A]] =
    Some(castToExpr[T, A](summon[ToExpr[T]]))

  // Helper for custom ToExpr definitions
  private inline def custom[T, A](te: ToExpr[T]): Option[ToExpr[A]] =
    Some(castToExpr[T, A](te))

  def summonExprOpt[A: Type](using Quotes): Option[ToExpr[A]] =
    Type.of[A] match
      case '[Int]     => primitive[Int, A]
      case '[String]  => primitive[String, A]
      case '[Boolean] => primitive[Boolean, A]
      case '[Long]    => primitive[Long, A]
      case '[Double]  => primitive[Double, A]
      case '[Float]   => primitive[Float, A]
      case '[Char]    => primitive[Char, A]
      case '[Byte]    => primitive[Byte, A]
      case '[Short]   => primitive[Short, A]
      case '[Unit] =>
        custom[Unit, A](
          new ToExpr[Unit]:
            def apply(u: Unit)(using Quotes): Expr[Unit] = '{ () }
        )
      case '[scala.math.BigInt] =>
        custom[BigInt, A](
          new ToExpr[BigInt]:
            def apply(b: BigInt)(using Quotes): Expr[BigInt] =
              '{ scala.math.BigInt(${ Expr(b.toString) }) }
        )
      case '[scala.math.BigDecimal] =>
        custom[BigDecimal, A](
          new ToExpr[BigDecimal]:
            def apply(b: BigDecimal)(using Quotes): Expr[BigDecimal] =
              '{ scala.math.BigDecimal(${ Expr(b.toString) }) }
        )
      case _ => None
