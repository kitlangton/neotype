package comptime

import scala.quoted.*

import MacroExprs.castToExprOpt

private[comptime] object MacroExprsCollections:
  def summonExprOpt[A: Type](using Quotes): Option[ToExpr[A]] =
    Type.of[A] match
      // Tuples (can't use collectionToExpr helper - need multiple element types)
      case '[(a, b)] =>
        castToExprOpt(for
          teA <- MacroExprs.summonExprOpt[a]
          teB <- MacroExprs.summonExprOpt[b]
        yield
          given ToExpr[a] = teA
          given ToExpr[b] = teB
          new ToExpr[(a, b)]:
            def apply(t: (a, b))(using Quotes): Expr[(a, b)] =
              '{ (${ Expr(t._1) }, ${ Expr(t._2) }) })

      case '[(a, b, c)] =>
        castToExprOpt(for
          teA <- MacroExprs.summonExprOpt[a]
          teB <- MacroExprs.summonExprOpt[b]
          teC <- MacroExprs.summonExprOpt[c]
        yield
          given ToExpr[a] = teA
          given ToExpr[b] = teB
          given ToExpr[c] = teC
          new ToExpr[(a, b, c)]:
            def apply(t: (a, b, c))(using Quotes): Expr[(a, b, c)] =
              '{ (${ Expr(t._1) }, ${ Expr(t._2) }, ${ Expr(t._3) }) })

      // Single-element collections (inline pattern required due to Quotes scoping and method application)
      case '[List[t]] =>
        castToExprOpt(MacroExprs.summonExprOpt[t].map { te =>
          given ToExpr[t] = te
          new ToExpr[List[t]]:
            def apply(xs: List[t])(using Quotes): Expr[List[t]] =
              Expr.ofList(xs.map(Expr(_)))
        })

      case '[Vector[t]] =>
        castToExprOpt(MacroExprs.summonExprOpt[t].map { te =>
          given ToExpr[t] = te
          new ToExpr[Vector[t]]:
            def apply(xs: Vector[t])(using Quotes): Expr[Vector[t]] =
              '{ ${ Expr.ofList(xs.toList.map(Expr(_))) }.toVector }
        })

      case '[Array[Byte]] =>
        castToExprOpt(
          Some(
            new ToExpr[Array[Byte]]:
              def apply(xs: Array[Byte])(using Quotes): Expr[Array[Byte]] =
                val elems = xs.toIndexedSeq.map(Expr(_))
                '{ Array[Byte](${ Varargs(elems) }*) }
          )
        )

      case '[Set[t]] =>
        castToExprOpt(MacroExprs.summonExprOpt[t].map { te =>
          given ToExpr[t] = te
          new ToExpr[Set[t]]:
            def apply(xs: Set[t])(using Quotes): Expr[Set[t]] =
              '{ ${ Expr.ofList(xs.toList.map(Expr(_))) }.toSet }
        })

      case '[scala.collection.Seq[t]] =>
        castToExprOpt(MacroExprs.summonExprOpt[t].map { te =>
          given ToExpr[t] = te
          new ToExpr[scala.collection.Seq[t]]:
            def apply(xs: scala.collection.Seq[t])(using Quotes): Expr[scala.collection.Seq[t]] =
              '{ ${ Expr.ofList(xs.toList.map(Expr(_))) }.toSeq }
        })

      case '[scala.collection.immutable.Seq[t]] =>
        castToExprOpt(MacroExprs.summonExprOpt[t].map { te =>
          given ToExpr[t] = te
          new ToExpr[scala.collection.immutable.Seq[t]]:
            def apply(xs: scala.collection.immutable.Seq[t])(using Quotes): Expr[scala.collection.immutable.Seq[t]] =
              '{ ${ Expr.ofList(xs.toList.map(Expr(_))) }.toSeq }
        })

      // Map needs two element types
      case '[Map[k, v]] =>
        castToExprOpt(for
          teK <- MacroExprs.summonExprOpt[k]
          teV <- MacroExprs.summonExprOpt[v]
        yield
          given ToExpr[k] = teK
          given ToExpr[v] = teV
          new ToExpr[Map[k, v]]:
            def apply(m: Map[k, v])(using Quotes): Expr[Map[k, v]] =
              val entries  = m.toList.map { case (k, v) => '{ (${ Expr(k) }, ${ Expr(v) }) } }
              val listExpr = Expr.ofList(entries)
              '{ $listExpr.toMap })

      case _ => None
