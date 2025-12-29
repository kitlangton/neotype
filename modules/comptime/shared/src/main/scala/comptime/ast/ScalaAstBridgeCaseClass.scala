package comptime

import scala.quoted.*

object ScalaAstBridgeCaseClass:
  def fromBase[Q <: Quotes](using
      quotes: Q
  )(
      base: quotes.reflect.Term,
      argss: List[List[quotes.reflect.Term]],
      mapArgs: (List[quotes.reflect.Term], List[String]) => List[TermIR]
  ): Option[TermIR] =
    import quotes.reflect.*

    def caseClassInfoFrom(sym: Symbol): Option[(String, List[String], Option[Int])] =
      if sym == Symbol.noSymbol then None
      else
        val cls =
          if sym.flags.is(Flags.Module) then sym.companionClass
          else sym
        if cls != Symbol.noSymbol && cls.flags.is(Flags.Case) then
          val fullName = cls.fullName
          if fullName.startsWith("scala.") || fullName.startsWith("java.") then None
          else
            val params = cls.primaryConstructor.paramSymss.flatten.filter(p => p.isTerm && !p.isType)
            val fields = params.map(_.name)
            def isRepeatedParam(sym: Symbol): Boolean =
              sym.tree match
                case v: ValDef => v.tpt.tpe.typeSymbol.fullName == "scala.<repeated>"
                case _         => false
            def repeatedIndexFrom(params: List[Symbol]): Option[Int] =
              params.indexWhere(isRepeatedParam) match
                case -1  => None
                case idx => Some(idx)
            val repeatedIndex =
              repeatedIndexFrom(params).orElse {
                val applyParams =
                  cls.companionModule.methodMembers
                    .find(_.name == "apply")
                    .map(_.paramSymss.flatten.filter(p => p.isTerm && !p.isType))
                    .getOrElse(Nil)
                repeatedIndexFrom(applyParams)
              }
            Some((fullName, fields, repeatedIndex))
        else None

    def caseClassArgs(fields: List[String]): Option[List[TermIR]] =
      argss match
        case Nil         => Some(Nil)
        case args :: Nil => Some(mapArgs(args, fields))
        case _           => None

    def caseClassFromBase(base: Term): Option[TermIR] =
      base match
        case Apply(Select(New(tpt), "<init>"), _) =>
          caseClassInfoFrom(tpt.tpe.typeSymbol).flatMap { case (fullName, fields, repeatedIndex) =>
            caseClassArgs(fields).map { argsIrs =>
              TermIR.CaseClass(fullName, fields, repeatedIndex, argsIrs)
            }
          }
        case Select(New(tpt), "<init>") =>
          caseClassInfoFrom(tpt.tpe.typeSymbol).flatMap { case (fullName, fields, repeatedIndex) =>
            caseClassArgs(fields).map { argsIrs =>
              TermIR.CaseClass(fullName, fields, repeatedIndex, argsIrs)
            }
          }
        case Select(recv, "apply") =>
          caseClassInfoFrom(recv.tpe.typeSymbol).flatMap { case (fullName, fields, repeatedIndex) =>
            caseClassArgs(fields).map { argsIrs =>
              TermIR.CaseClass(fullName, fields, repeatedIndex, argsIrs)
            }
          }
        case _ => None

    caseClassFromBase(base)
