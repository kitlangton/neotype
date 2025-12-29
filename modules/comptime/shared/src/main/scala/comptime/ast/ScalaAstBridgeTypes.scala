package comptime

import scala.quoted.*

object ScalaAstBridgeTypes:
  def typeToIR[Q <: Quotes](using quotes: Q)(tpe: quotes.reflect.TypeRepr): TypeIR =
    import quotes.reflect.*
    val sym = tpe.typeSymbol
    if sym == Symbol.noSymbol then TypeIR.AnyType()
    else
      tpe.dealias match
        case AppliedType(_, args) =>
          TypeIR.Ref(sym.fullName, args.map(typeToIR))
        case _ =>
          TypeIR.Ref(sym.fullName, Nil)
