package comptime

import scala.quoted.*

private[comptime] object ScalaAstBridgeStatements:
  def statementToIR[Q <: Quotes](using
      quotes: Q
  )(
      stmt: quotes.reflect.Statement,
      termToIR: quotes.reflect.Term => TermIR,
      typeToIR: quotes.reflect.TypeRepr => TypeIR
  ): TermIR =
    import quotes.reflect.*
    stmt match
      case ValDef(name, _, Some(rhs)) =>
        TermIR.Val(name, termToIR(rhs))
      case DefDef(name, params, _, Some(rhs)) =>
        val paramIrs = params.flatMap(_.params).map {
          case v: ValDef => ParamIR(v.name, typeToIR(v.tpt.tpe))
          case other =>
            throw new NotImplementedError(s"ScalaAstBridge.statementToIR (param): ${other.show}")
        }
        TermIR.Val(name, TermIR.Lambda(paramIrs, termToIR(rhs)))
      case ValDef(name, _, None) =>
        throw new NotImplementedError(s"ScalaAstBridge.statementToIR (val without rhs): $name")
      case t: Term => termToIR(t)
      case other =>
        throw new NotImplementedError(s"ScalaAstBridge.statementToIR: ${other.show}")

  def caseDefToIR[Q <: Quotes](using
      quotes: Q
  )(
      caseDef: quotes.reflect.CaseDef,
      termToIR: quotes.reflect.Term => TermIR,
      patternToIR: quotes.reflect.Tree => PatternIR
  ): CaseIR =
    val guard = caseDef.guard.map(termToIR)
    CaseIR(patternToIR(caseDef.pattern), guard, termToIR(caseDef.rhs))

  def refFromIdent[Q <: Quotes](using quotes: Q)(id: quotes.reflect.Ident): TermIR.Ref =
    val sym = id.symbol
    val fullName =
      if sym == quotes.reflect.Symbol.noSymbol then None else Some(sym.fullName)
    TermIR.Ref(id.name, fullName)
