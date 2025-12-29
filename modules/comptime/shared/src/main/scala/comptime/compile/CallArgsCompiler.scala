package comptime

object CallArgsCompiler:
  def compileArgs(
      terms: List[TermIR],
      env: Map[String, Eval],
      fold: Boolean
  )(
      compileTerm: (TermIR, Map[String, Eval], Boolean) => Either[ComptimeError, Eval]
  ): Either[ComptimeError, List[Eval]] =
    util.Args.compileArgsList(terms, term => compileTerm(term, env, fold))
