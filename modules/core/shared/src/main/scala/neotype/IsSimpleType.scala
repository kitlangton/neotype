package neotype

import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import scala.quoted.*

@implicitNotFound("${A} has a `validate` method")
trait IsSimpleType[A]

object IsSimpleType:
  transparent inline given [A <: TypeWrapper[?]]: IsSimpleType[A] = ${
    isSimpleTypeImpl[A]
  }

  private def isSimpleTypeImpl[A: Type](using
      Quotes
  ): Expr[IsSimpleType[A]] =
    import quotes.reflect.*

    val (name, isValidated) = hasValidateMethod[A]
    if isValidated then report.errorAndAbort(s"Newtype $name is not a simple type")
    else '{ new IsSimpleType[A] {} }

@implicitNotFound("${A} does not have a `validate` method")
trait IsValidatedType[A]

object IsValidatedType:
  transparent inline given [A <: TypeWrapper[?]]: IsValidatedType[A] = ${
    isValidatedTypeImpl[A]
  }

  private def isValidatedTypeImpl[A: Type](using
      Quotes
  ): Expr[IsValidatedType[A]] =
    import quotes.reflect.*

    val (name, isValidated) = hasValidateMethod[A]
    if !isValidated then report.errorAndAbort(s"Newtype $name is not a validated type")
    else '{ new IsValidatedType[A] {} }

///////////////////
// Helper Method //
///////////////////

private def hasValidateMethod[A: Type](using Quotes): (String, Boolean) =
  import quotes.reflect.*

  @tailrec
  def getNewtype(t: TypeRepr): TypeRepr = t.widenTermRefByName match
    case Refinement(t, _, _) => t
    case AndType(_, t2)      => getNewtype(t2)
    case t                   => t

  lazy val nt = getNewtype(TypeRepr.of[A])
  (nt.show, nt.typeSymbol.declaredMethods.exists(_.name == "validate"))
