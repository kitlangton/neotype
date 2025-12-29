package comptime

import scala.quoted.*

private[comptime] object ScalaAstBridgeTerms:
  def termToIR[Q <: Quotes](using quotes: Q)(term0: quotes.reflect.Term): TermIR =
    import quotes.reflect.*

    def typeToIR(tpe: TypeRepr): TypeIR =
      ScalaAstBridgeTypes.typeToIR(tpe)

    def patternToIR(pat: Tree): PatternIR =
      ScalaAstBridgePatterns.patternToIR(pat, termToIR0, typeToIR)

    def termToIR0(term: Term): TermIR =
      ScalaAstBridgeNormalize.normalize(term) match
        case Literal(constant) =>
          TermIR.Lit(constant.value)
        case New(tpt) =>
          TermIR.Ref(tpt.tpe.typeSymbol.name, Some(tpt.tpe.typeSymbol.fullName))
        case id: Ident =>
          ScalaAstBridgeStatements.refFromIdent(id)
        case Block(stats, expr) =>
          TermIR.Block(
            stats.map(stmt => ScalaAstBridgeStatements.statementToIR(stmt, termToIR0, typeToIR)),
            termToIR0(expr)
          )
        case sel: Select if sel.symbol.flags.is(Flags.Module) =>
          TermIR.Ref(sel.name, Some(sel.symbol.fullName))
        // Enum case access: Row.Top, Col.Left, etc.
        // Note: Only works for enums defined in a separate compilation unit
        case sel: Select if sel.symbol.flags.is(Flags.Enum) =>
          TermIR.Ref(sel.name, Some(sel.symbol.fullName))
        case sel @ Select(recv, name) =>
          val recvIR = termToIR0(recv)
          val owner  = ScalaAstBridgeCall.callOwner(term, Some(recv))
          val pos    = ScalaAstBridgePos.extractPos(sel)
          TermIR.Call(CallIR(recvIR, owner, name, Nil, Nil, pos))
        case Match(scrutinee, cases) =>
          TermIR.Match(
            termToIR0(scrutinee),
            cases.map(cd => ScalaAstBridgeStatements.caseDefToIR(cd, termToIR0, patternToIR))
          )
        case Try(expr, cases, finalizer) =>
          TermIR.Try(
            termToIR0(expr),
            cases.map(cd => ScalaAstBridgeStatements.caseDefToIR(cd, termToIR0, patternToIR)),
            finalizer.map(termToIR0)
          )
        case If(cond, onTrue, onFalse) =>
          TermIR.If(termToIR0(cond), termToIR0(onTrue), termToIR0(onFalse))
        case Lambda(params, body) =>
          val paramIrs = params.map(p => ParamIR(p.name, typeToIR(p.tpt.tpe)))
          TermIR.Lambda(paramIrs, termToIR0(body))
        case Closure(meth, _) =>
          termToIR0(meth)
        case term =>
          val (base, targs, argss) = ScalaAstBridgeCall.peel(term)
          ScalaAstBridgeWrapper.unwrapTypeWrapperApply(base, argss, termToIR0).getOrElse {
            def mapArgs(args: List[Term], paramNames: List[String]): List[TermIR] =
              ScalaAstBridgeArgs.mapArgs(args, paramNames, ScalaAstBridgeNormalize.normalize, termToIR0)

            def caseClassFromBase(base: Term): Option[TermIR] =
              ScalaAstBridgeCaseClass.fromBase(base, argss, mapArgs)

            val baseNoTpe = ScalaAstBridgeWrapper.stripTypeApply(base)
            caseClassFromBase(baseNoTpe).getOrElse {
              ScalaAstBridgeCallBuild.buildCall(
                baseNoTpe,
                targs,
                argss,
                termToIR0,
                typeToIR,
                mapArgs
              )
            }
          }

    termToIR0(term0)
