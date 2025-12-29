package comptime

import scala.quoted.*

private[comptime] object ScalaAstBridgeWrapper:
  def unwrapTypeWrapperApply[Q <: Quotes](using
      quotes: Q
  )(
      base: quotes.reflect.Term,
      argss: List[List[quotes.reflect.Term]],
      termToIR: quotes.reflect.Term => TermIR
  ): Option[TermIR] =
    import quotes.reflect.*

    def isTypeWrapper(tpe: TypeRepr): Boolean =
      tpe.baseClasses.exists { sym =>
        sym.fullName == "neotype.TypeWrapper" ||
        sym.fullName == "neotype.Newtype" ||
        sym.fullName == "neotype.Subtype"
      } ||
        tpe.typeSymbol.methodMembers.exists(_.name == "validate")

    def isTypeWrapperApply(sym: Symbol): Boolean =
      sym.owner.fullName == "neotype.TypeWrapper" ||
        sym.owner.fullName == "neotype.Newtype" ||
        sym.owner.fullName == "neotype.Subtype"

    argss match
      case List(List(arg)) =>
        base match
          case Select(recv, "apply") if isTypeWrapper(recv.tpe) || isTypeWrapperApply(base.symbol) =>
            Some(termToIR(arg))
          case id: Ident if isTypeWrapper(id.tpe) =>
            Some(termToIR(arg))
          case _ => None
      case _ => None

  def stripTypeApply[Q <: Quotes](using quotes: Q)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case TypeApply(fun, _) => stripTypeApply(fun)
      case other             => other
