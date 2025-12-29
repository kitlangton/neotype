package comptime

import scala.quoted.*

object ScalaAstBridgeCall:
  def callOwner[Q <: Quotes](using quotes: Q)(fun: quotes.reflect.Term, recvOpt: Option[quotes.reflect.Term]): String =
    import quotes.reflect.*
    recvOpt match
      case Some(recv) =>
        val widened = recv.tpe.widenTermRefByName.dealias
        widened.classSymbol match
          case Some(sym) => sym.fullName
          case None =>
            val sym = widened.typeSymbol
            if sym == Symbol.noSymbol then
              widened match
                case AppliedType(tcon, _) => tcon.typeSymbol.fullName
                case _                    => recv.symbol.fullName
            else sym.fullName
      case None =>
        fun.symbol.owner.fullName

  def peel[Q <: Quotes](using
      quotes: Q
  )(term: quotes.reflect.Term): (quotes.reflect.Term, List[quotes.reflect.TypeTree], List[List[quotes.reflect.Term]]) =
    import quotes.reflect.*
    term match
      case Apply(fun, args) =>
        val (base, targs, argss) = peel(fun)
        val expandedArgs = args.flatMap {
          case Repeated(elems, _) => elems
          case other              => List(other)
        }
        if expandedArgs.isEmpty then (base, targs, argss)
        else (base, targs, argss :+ expandedArgs)
      case TypeApply(fun, targs) =>
        val (base, targs0, argss) = peel(fun)
        (base, targs0 ++ targs, argss)
      case _ =>
        (term, Nil, Nil)
