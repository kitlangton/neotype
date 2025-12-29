package comptime

private[comptime] object BlockCompiler:
  def compileBlock(
      stats: List[TermIR],
      expr: TermIR,
      env: Map[String, Eval],
      fold: Boolean
  )(
      compileTerm: (TermIR, Map[String, Eval], Boolean) => Either[ComptimeError, Eval]
  ): Either[ComptimeError, Eval] =
    def loop(remaining: List[TermIR], curEnv: Map[String, Eval]): Either[ComptimeError, Map[String, Eval]] =
      remaining match
        case Nil => Right(curEnv)
        case TermIR.Val(name, value) :: tail =>
          compileTerm(value, curEnv, fold).flatMap { eval =>
            loop(tail, curEnv.updated(name, eval))
          }
        case other :: tail =>
          compileTerm(other, curEnv, fold).flatMap(_ => loop(tail, curEnv))

    loop(stats, env).flatMap { newEnv =>
      compileTerm(expr, newEnv, fold)
    }
