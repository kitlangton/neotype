package neotype.eval

import scala.quoted.*
import scala.annotation.nowarn

object CustomFromExpr:
  given fromExprSet[A]: FromExpr[Set[A]] with
    def unapply(x: Expr[Set[A]])(using Quotes) =
      import quotes.reflect.*
      val aType = x.asTerm.tpe.widen.typeArgs.head.asType
      fromExprForType[A](aType) match
        case None => None
        case Some(fromExprA) =>
          given FromExpr[A] = fromExprA
          @nowarn("msg=unused")
          given Type[A] = aType.asInstanceOf[Type[A]]
          x match
            case '{ Set[A](${ Varargs(Exprs(elems)) }*) }                            => Some(elems.toSet)
            case '{ Set.empty[A] }                                                   => Some(Set.empty[A])
            case '{ scala.collection.immutable.Set[A](${ Varargs(Exprs(elems)) }*) } => Some(elems.toSet)
            case '{ scala.collection.immutable.Set.empty[A] }                        => Some(Set.empty[A])
            case _                                                                   => None

  given fromExprList[A]: FromExpr[List[A]] with
    def unapply(x: Expr[List[A]])(using Quotes) =
      import quotes.reflect.*
      val aType = x.asTerm.tpe.widen.typeArgs.head.asType
      fromExprForType[A](aType) match
        case None => None
        case Some(fromExprA) =>
          given FromExpr[A] = fromExprA
          @nowarn("msg=unused")
          given Type[A] = aType.asInstanceOf[Type[A]]
          x match
            case '{ List[A](${ Varargs(Exprs(elems)) }*) }                            => Some(elems.toList)
            case '{ List.empty[A] }                                                   => Some(List.empty[A])
            case '{ scala.collection.immutable.List[A](${ Varargs(Exprs(elems)) }*) } => Some(elems.toList)
            case '{ scala.collection.immutable.List.empty[A] }                        => Some(List.empty[A])
            case _                                                                    => None

  given fromExprVector[A]: FromExpr[Vector[A]] with
    def unapply(x: Expr[Vector[A]])(using Quotes) =
      import quotes.reflect.*
      val aType = x.asTerm.tpe.widen.typeArgs.head.asType
      fromExprForType[A](aType) match
        case None => None
        case Some(fromExprA) =>
          given FromExpr[A] = fromExprA
          @nowarn("msg=unused")
          given Type[A] = aType.asInstanceOf[Type[A]]
          x match
            case '{ Vector[A](${ Varargs(Exprs(elems)) }*) }                            => Some(elems.toVector)
            case '{ Vector.empty[A] }                                                   => Some(Vector.empty[A])
            case '{ scala.collection.immutable.Vector[A](${ Varargs(Exprs(elems)) }*) } => Some(elems.toVector)
            case '{ scala.collection.immutable.Vector.empty[A] }                        => Some(Vector.empty[A])
            case _                                                                      => None

  given fromExprOption[A]: FromExpr[Option[A]] with
    def unapply(x: Expr[Option[A]])(using Quotes) =
      import quotes.reflect.*
      val aType = x.asTerm.tpe.widen.typeArgs.head.asType
      fromExprForType[A](aType) match
        case None => None
        case Some(fromExprA) =>
          given FromExpr[A] = fromExprA
          @nowarn("msg=unused")
          given Type[A] = aType.asInstanceOf[Type[A]]
          x match
            case '{ Option[A](${ Expr(a) }) } => Some(Some(a))
            case '{ Some[A](${ Expr(a) }) }   => Some(Some(a))
            case '{ None }                    => Some(None)
            case _                            => None

  def fromExprForType[A](using Quotes)(tpe: Type[?]): Option[FromExpr[A]] =
    import quotes.reflect.*
    val result = tpe match
      case '[String]  => Some(summon[FromExpr[String]])
      case '[String]  => Some(summon[FromExpr[Int]])
      case '[Long]    => Some(summon[FromExpr[Long]])
      case '[Short]   => Some(summon[FromExpr[Short]])
      case '[Char]    => Some(summon[FromExpr[Char]])
      case '[Byte]    => Some(summon[FromExpr[Byte]])
      case '[Double]  => Some(summon[FromExpr[Double]])
      case '[Float]   => Some(summon[FromExpr[Float]])
      case '[Boolean] => Some(summon[FromExpr[Boolean]])
      case '[Nothing] => Some(???)
      case '[other]   => None
    result.asInstanceOf[Option[FromExpr[A]]]
    // report.errorAndAbort(s"Could not find FromExpr for type: ${Type.show[other]}")
