package comptime

object MatchCompiler:
  def compileMatch(
      value: Any,
      cases: List[CaseIR],
      env: Map[String, Eval],
      fold: Boolean
  )(
      compileTerm: (TermIR, Map[String, Eval], Boolean) => Either[ComptimeError, Eval]
  ): Either[ComptimeError, Eval] =
    def loop(remaining: List[CaseIR]): Either[ComptimeError, Eval] =
      remaining match
        case Nil =>
          Left(ComptimeFailure.MatchError("no cases matched"))
        case CaseIR(pattern, guard, body) :: tail =>
          val evalPatternTerm: TermIR => Either[ComptimeError, Any] =
            term => compileTerm(term, env, fold).map(Eval.run)
          PatternEngine.matches(pattern, value, evalPatternTerm) match
            case Left(err)   => Left(err)
            case Right(None) => loop(tail)
            case Right(Some(binds)) =>
              val env2 = env ++ binds.view.mapValues(v => Eval.Value(v)).toMap
              val guardOkE =
                guard match
                  case None => Right(true)
                  case Some(g) =>
                    compileTerm(g, env2, fold).map { ev =>
                      Eval.run(ev).asInstanceOf[Boolean]
                    }
              guardOkE.flatMap { ok =>
                if ok then compileTerm(body, env2, fold) else loop(tail)
              }

    loop(cases)
