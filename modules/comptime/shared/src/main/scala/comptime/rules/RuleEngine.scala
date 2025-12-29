package comptime

final class RuleEngine(rules: List[RuleSpec]):
  private val callRules: List[CallRule]           = rules.collect { case r: CallRule => r }
  private val callIndex: CallRuleEngine.RuleIndex = CallRuleEngine.index(callRules)

  def compileCall(call: CallIR, ctx: RuleContext): Either[ComptimeError, Eval] =
    CallRuleEngine.compileCall(call, ctx, callIndex)

  def compileTerm(term: TermIR): Either[ComptimeError, Eval] =
    TermCompiler.compileTerm(term, Map.empty, fold = true, compileCall)
