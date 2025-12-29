package comptime

import scala.quoted.*

private[comptime] object ScalaAstBridgeCallBuild:
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
    TermIR.Call(CallIR(recv, owner, name, targsIR, List(mappedArgs), pos))
