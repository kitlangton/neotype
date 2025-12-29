package comptime

import scala.quoted.*

private[comptime] object ScalaAstBridgePatterns:
  def patternToIR[Q <: Quotes](using
      quotes: Q
  )(
      pat: quotes.reflect.Tree,
      termToIR0: quotes.reflect.Term => TermIR,
      typeToIR: quotes.reflect.TypeRepr => TypeIR
  ): PatternIR =
    import quotes.reflect.*

    def isSeqTpt(tpt: TypeTree): Boolean =
      tpt.tpe <:< TypeRepr.of[Seq[Any]]

    def ownerFromFun(fun: Tree): String =
      fun match
        case Select(qual, _) =>
          if qual.symbol.isPackageDef then fun.symbol.fullName else qual.tpe.typeSymbol.fullName
        case Ident(_) =>
          val sym = fun.symbol
          if sym.flags.is(Flags.Method) then sym.owner.fullName else sym.fullName
        case _ => fun.symbol.owner.fullName

    def recvFromFun(fun: Tree): Option[TermIR] =
      fun match
        case Select(qual, _) if !qual.symbol.isPackageDef && !qual.symbol.flags.is(Flags.Module) =>
          Some(termToIR0(qual))
        case _ => None

    pat match
      case Wildcard() =>
        PatternIR.Wildcard()
      case Bind(name, inner) =>
        patternToIR(inner, termToIR0, typeToIR) match
          case PatternIR.SeqWildcard(_) => PatternIR.SeqWildcard(Some(name))
          case other                    => PatternIR.Bind(name, other)
      case Literal(constant) =>
        PatternIR.Literal(constant.value)
      case TypedOrTest(inner, tpt) =>
        val isRepeated = tpt.tpe.typeSymbol.fullName == "scala.<repeated>"
        if isRepeated then PatternIR.SeqWildcard(None)
        else if isSeqTpt(tpt) then
          inner match
            case Bind(name, Wildcard()) => PatternIR.SeqWildcard(Some(name))
            case Wildcard()             => PatternIR.SeqWildcard(None)
            case _                      => PatternIR.Typed(typeToIR(tpt.tpe), patternToIR(inner, termToIR0, typeToIR))
        else PatternIR.Typed(typeToIR(tpt.tpe), patternToIR(inner, termToIR0, typeToIR))
      case Alternatives(patterns) =>
        PatternIR.Alt(patterns.map(patternToIR(_, termToIR0, typeToIR)))
      case Apply(fun, args) =>
        val owner = ownerFromFun(fun)
        val recv  = recvFromFun(fun)
        PatternIR.Unapply(owner, recv, args.map(patternToIR(_, termToIR0, typeToIR)), isSeq = false)
      case Ident(name) =>
        if name == "_" then PatternIR.Wildcard()
        else if name.nonEmpty && name.head.isLower then PatternIR.Bind(name, PatternIR.Wildcard())
        else PatternIR.Unapply(pat.symbol.fullName, None, Nil, isSeq = false)
      case Unapply(fun, _, patterns) =>
        val owner = ownerFromFun(fun)
        val recv  = recvFromFun(fun)
        val isSeq = fun.symbol.name == "unapplySeq"
        if owner.startsWith("scala.Tuple") then PatternIR.Tuple(patterns.map(patternToIR(_, termToIR0, typeToIR)))
        else PatternIR.Unapply(owner, recv, patterns.map(patternToIR(_, termToIR0, typeToIR)), isSeq)
      case Inlined(_, _, p) =>
        patternToIR(p, termToIR0, typeToIR)
      case other =>
        throw new NotImplementedError(s"ScalaAstBridge.patternToIR: [${other.getClass.getName}]")
