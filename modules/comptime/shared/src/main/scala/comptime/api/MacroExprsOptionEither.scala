package comptime

import scala.quoted.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import MacroExprs.castToExprOpt

object MacroExprsOptionEither:
  def summonExprOpt[A: Type](using Quotes): Option[ToExpr[A]] =
    Type.of[A] match
      case '[Option[t]] =>
        castToExprOpt(MacroExprs.summonExprOpt[t].map { te =>
          given ToExpr[t] = te
          new ToExpr[Option[t]]:
            def apply(opt: Option[t])(using Quotes): Expr[Option[t]] =
              opt match
                case Some(value) => '{ Some(${ Expr(value) }) }
                case None        => '{ None }
        })

      case '[Some[t]] =>
        castToExprOpt(MacroExprs.summonExprOpt[t].map { te =>
          given ToExpr[t] = te
          new ToExpr[Some[t]]:
            def apply(opt: Some[t])(using Quotes): Expr[Some[t]] =
              opt match
                case Some(value) => '{ Some(${ Expr(value) }) }
        })

      case '[Either[l, r]] =>
        castToExprOpt(for
          teL <- MacroExprs.summonExprOpt[l]
          teR <- MacroExprs.summonExprOpt[r]
        yield
          given ToExpr[l] = teL
          given ToExpr[r] = teR
          new ToExpr[Either[l, r]]:
            def apply(e: Either[l, r])(using Quotes): Expr[Either[l, r]] =
              e match
                case Left(value)  => '{ Left[l, r](${ Expr(value) }) }
                case Right(value) => '{ Right[l, r](${ Expr(value) }) })

      case '[Left[l, r]] =>
        castToExprOpt(MacroExprs.summonExprOpt[l].map { te =>
          given ToExpr[l] = te
          new ToExpr[Left[l, r]]:
            def apply(e: Left[l, r])(using Quotes): Expr[Left[l, r]] =
              e match
                case Left(value) => '{ Left[l, r](${ Expr(value) }) }
        })

      case '[Right[l, r]] =>
        castToExprOpt(MacroExprs.summonExprOpt[r].map { te =>
          given ToExpr[r] = te
          new ToExpr[Right[l, r]]:
            def apply(e: Right[l, r])(using Quotes): Expr[Right[l, r]] =
              e match
                case Right(value) => '{ Right[l, r](${ Expr(value) }) }
        })

      case '[Try[t]] =>
        castToExprOpt(MacroExprs.summonExprOpt[t].map { te =>
          given ToExpr[t] = te
          new ToExpr[Try[t]]:
            def apply(tr: Try[t])(using Quotes): Expr[Try[t]] =
              tr match
                case Success(value) => '{ Success(${ Expr(value) }) }
                case Failure(ex) =>
                  val msg       = Expr(Option(ex.getMessage).getOrElse(""))
                  val className = Expr(ex.getClass.getName)
                  '{ Failure(new RuntimeException("[" + $className + "] " + $msg)) }
        })

      case '[Success[t]] =>
        castToExprOpt(MacroExprs.summonExprOpt[t].map { te =>
          given ToExpr[t] = te
          new ToExpr[Success[t]]:
            def apply(s: Success[t])(using Quotes): Expr[Success[t]] =
              s match
                case Success(value) => '{ Success(${ Expr(value) }) }
        })

      case _ => None
