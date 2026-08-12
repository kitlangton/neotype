package comptime

import scala.quoted.*

private[comptime] object ScalaAstBridgeCallBuild:
  // Since Scala 3.10 the `Predef.intWrapper`-style conversions to the `scala.runtime.Rich*`
  // value classes are extension methods on the primitive companions, so `3.max(5)` types as
  // `Int.max(3)(5)` rather than `intWrapper(3).max(5)`. Rewriting those calls back into the
  // conversion shape keeps a single IR shape across compiler versions.
  private val primitiveExtensions: Map[String, (String, String)] = Map(
    "scala.Int"    -> ("intWrapper", "scala.runtime.RichInt"),
    "scala.Long"   -> ("longWrapper", "scala.runtime.RichLong"),
    "scala.Float"  -> ("floatWrapper", "scala.runtime.RichFloat"),
    "scala.Double" -> ("doubleWrapper", "scala.runtime.RichDouble"),
    "scala.Char"   -> ("charWrapper", "scala.runtime.RichChar"),
    "scala.Byte"   -> ("byteWrapper", "scala.runtime.RichByte"),
    "scala.Short"  -> ("shortWrapper", "scala.runtime.RichShort")
  )

  private val predefOwner = "scala.Predef$"
  private val predefRef   = TermIR.Ref("Predef", Some(predefOwner))

  def buildCall[Q <: Quotes](using
      quotes: Q
  )(
      base: quotes.reflect.Term,
      targs: List[quotes.reflect.TypeTree],
      argss: List[List[quotes.reflect.Term]],
      termToIR: quotes.reflect.Term => TermIR,
      typeToIR: quotes.reflect.TypeRepr => TypeIR,
      mapArgs: (List[quotes.reflect.Term], List[String]) => List[TermIR]
  ): TermIR =
    import quotes.reflect.*

    val (targsIR, paramNames) =
      base.symbol.paramSymss match
        case _ :: vparams :: _ =>
          val params = vparams.filter(p => p.isTerm && !p.isType).map(_.name)
          (targs.map(t => typeToIR(t.tpe)), params)
        case vparams :: Nil =>
          val params = vparams.filter(p => p.isTerm && !p.isType).map(_.name)
          (targs.map(t => typeToIR(t.tpe)), params)
        case _ =>
          (targs.map(t => typeToIR(t.tpe)), Nil)

    val (owner, name, recv) =
      base match
        case ident: Ident if ident.symbol.flags.is(Flags.Module) =>
          (ident.symbol.fullName, "apply", TermIR.Ref(ident.name, Some(ident.symbol.fullName)))
        case Select(recv, _) =>
          (base.symbol.owner.fullName, base.symbol.name, termToIR(recv))
        // Handle Ident referring to a method on a companion object (e.g., implicit conversions)
        // The receiver is the method's owner (the companion object)
        case ident: Ident if ident.symbol.flags.is(Flags.Method) =>
          val ownerSym  = ident.symbol.owner
          val ownerName = ownerSym.name.stripSuffix("$")
          (ownerSym.fullName, ident.symbol.name, TermIR.Ref(ownerName, Some(ownerSym.fullName)))
        case _ =>
          (base.symbol.owner.fullName, base.symbol.name, TermIR.Ref("<none>", None))

    // Special handling for throw expressions (represented as <special-ops>.throw in Scala 3)
    if owner == "<special-ops>" && name == "throw" then
      val throwArg = argss.flatten.headOption.map(termToIR).getOrElse(TermIR.Lit(null))
      return TermIR.Throw(throwArg)

    val mappedArgs = argss.flatMap(args => mapArgs(args, paramNames))
    val pos        = ScalaAstBridgePos.extractPos(base)

    val richWrapper =
      if base.symbol.flags.is(Flags.ExtensionMethod) then primitiveExtensions.get(util.TypeNames.stripModule(owner))
      else None

    richWrapper match
      case Some((wrapper, richOwner)) if mappedArgs.nonEmpty =>
        val self = TermIR.Call(CallIR(predefRef, predefOwner, wrapper, Nil, List(List(mappedArgs.head)), pos))
        TermIR.Call(CallIR(self, richOwner, name, targsIR, List(mappedArgs.tail), pos))
      case _ =>
        TermIR.Call(CallIR(recv, owner, name, targsIR, List(mappedArgs), pos))
