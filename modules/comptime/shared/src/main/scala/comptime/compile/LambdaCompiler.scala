package comptime

private[comptime] object LambdaCompiler:
  def compileLambda(
      params: List[ParamIR],
      body: TermIR,
      env: Map[String, Eval]
  )(
      compileTerm: (TermIR, Map[String, Eval], Boolean) => Either[ComptimeError, Eval],
      compileMatch: (Any, List[CaseIR], Map[String, Eval], Boolean) => Either[ComptimeError, Eval]
  ): Either[ComptimeError, Eval] =
    params match
      case ParamIR(name, _) :: Nil =>
        body match
          case TermIR.Match(_, cases) =>
            val pf = new PartialFunction[Any, Any]:
              def isDefinedAt(arg: Any): Boolean =
                cases.exists { case CaseIR(pattern, guard, _) =>
                  val env2 = env.updated(name, Eval.Value(arg))
                  val evalPatternTerm: TermIR => Either[ComptimeError, Any] =
                    term => compileTerm(term, env2, true).map(Eval.run)
                  PatternEngine.matches(pattern, arg, evalPatternTerm) match
                    case Right(Some(binds)) =>
                      val env3 = env2 ++ binds.view.mapValues(v => Eval.Value(v)).toMap
                      guard match
                        case None => true
                        case Some(g) =>
                          compileTerm(g, env3, true) match
                            case Right(ev) => Eval.run(ev).asInstanceOf[Boolean]
                            case Left(_)   => false
                    case _ => false
                }
              def apply(arg: Any): Any =
                val env2 = env.updated(name, Eval.Value(arg))
                compileMatch(arg, cases, env2, true) match
                  case Right(eval) => Eval.run(eval)
                  case Left(err)   => throw new RuntimeException(ComptimeError.format(err))
            Right(Eval.Value(pf))
          case _ =>
            val fn: Any => Any = (arg: Any) =>
              val env2 = env.updated(name, Eval.Value(arg))
              compileTerm(body, env2, true) match
                case Right(eval) => Eval.run(eval)
                case Left(err)   => throw new RuntimeException(ComptimeError.format(err))
            Right(Eval.Value(fn))
      case ParamIR(n1, _) :: ParamIR(n2, _) :: Nil =>
        val fn: (Any, Any) => Any = (a: Any, b: Any) =>
          val env2 = env.updated(n1, Eval.Value(a)).updated(n2, Eval.Value(b))
          compileTerm(body, env2, true) match
            case Right(eval) => Eval.run(eval)
            case Left(err)   => throw new RuntimeException(ComptimeError.format(err))
        Right(Eval.Value(fn))
      case ParamIR(n1, _) :: ParamIR(n2, _) :: ParamIR(n3, _) :: Nil =>
        val fn: (Any, Any, Any) => Any = (a: Any, b: Any, c: Any) =>
          val env2 =
            env
              .updated(n1, Eval.Value(a))
              .updated(n2, Eval.Value(b))
              .updated(n3, Eval.Value(c))
          compileTerm(body, env2, true) match
            case Right(eval) => Eval.run(eval)
            case Left(err)   => throw new RuntimeException(ComptimeError.format(err))
        Right(Eval.Value(fn))
      case Nil =>
        val thunk: () => Any = () =>
          compileTerm(body, env, true) match
            case Right(eval) => Eval.run(eval)
            case Left(err)   => throw new RuntimeException(ComptimeError.format(err))
        Right(Eval.Value(thunk))
      case _ =>
        Left(ComptimeError.UnsupportedLambda(params.size, 3))
