package comptime

object Compiler:
  private val engine = new RuleEngine(StdlibRules.rules)

  def compileTerm(term: TermIR): Either[ComptimeError, Eval] =
    engine.compileTerm(term)
