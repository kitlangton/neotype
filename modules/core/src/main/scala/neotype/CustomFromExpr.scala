package neotype

import scala.quoted.*

object CustomFromExpr:
  given [A]: FromExpr[Set[A]] with
    def unapply(x: Expr[Set[A]])(using Quotes) =
      import quotes.reflect.*
      val aType         = x.asTerm.tpe.widen.typeArgs.head.asType
      given FromExpr[A] = fromExprForType(aType).asInstanceOf[FromExpr[A]]
      given Type[A]     = aType.asInstanceOf[Type[A]]
      x match
        case '{ Set[A](${ Varargs(Exprs(elems)) }*) }                            => Some(elems.toSet)
        case '{ Set.empty[A] }                                                   => Some(Set.empty[A])
        case '{ scala.collection.immutable.Set[A](${ Varargs(Exprs(elems)) }*) } => Some(elems.toSet)
        case '{ scala.collection.immutable.Set.empty[A] }                        => Some(Set.empty[A])
        case _                                                                   =>
//          report.warning(s"Cannot unapply Set from ${x}\n${x.asTerm}")
          None

  given [A]: FromExpr[List[A]] with
    def unapply(x: Expr[List[A]])(using Quotes) =
      import quotes.reflect.*
      val aType         = x.asTerm.tpe.widen.typeArgs.head.asType
      given FromExpr[A] = fromExprForType(aType).asInstanceOf[FromExpr[A]]
      given Type[A]     = aType.asInstanceOf[Type[A]]
      x match
        case '{ List[A](${ Varargs(Exprs(elems)) }*) }                            => Some(elems.toList)
        case '{ List.empty[A] }                                                   => Some(List.empty[A])
        case '{ scala.collection.immutable.List[A](${ Varargs(Exprs(elems)) }*) } => Some(elems.toList)
        case '{ scala.collection.immutable.List.empty[A] }                        => Some(List.empty[A])
        case _                                                                    =>
//          report.warning(s"Cannot unapply List from ${x.show}")
          None

  def fromExprForType(using Quotes)(tpe: Type[?]) =
    tpe match
      case '[String]  => summon[FromExpr[String]]
      case '[Int]     => summon[FromExpr[Int]]
      case '[Long]    => summon[FromExpr[Long]]
      case '[Short]   => summon[FromExpr[Short]]
      case '[Char]    => summon[FromExpr[Char]]
      case '[Byte]    => summon[FromExpr[Byte]]
      case '[Double]  => summon[FromExpr[Double]]
      case '[Float]   => summon[FromExpr[Float]]
      case '[Boolean] => summon[FromExpr[Boolean]]
