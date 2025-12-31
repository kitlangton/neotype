package comptime

private[comptime] object FunctionCallCompiler:
  def compileFunctionCall(
      call: CallIR,
      env: Map[String, Eval],
      fold: Boolean
  )(
      compileTerm: (TermIR, Map[String, Eval], Boolean) => Either[ComptimeError, Eval]
  ): Option[Either[ComptimeError, Eval]] =
    def isFunctionValue(eval: Eval): Boolean =
      eval match
        case Eval.Value(_: Function0[?])          => true
        case Eval.Value(_: Function1[?, ?])       => true
        case Eval.Value(_: Function2[?, ?, ?])    => true
        case Eval.Value(_: Function3[?, ?, ?, ?]) => true
        case _                                    => false

    // Handle both `f()` (call.name == name) and `f.apply()` (call.name == "apply") syntax
    call.recv match
      case TermIR.Ref(name, _)
          if (call.name == name || call.name == "apply") && env.get(name).exists(isFunctionValue) =>
        val fnEval       = env(name)
        val argTerms     = call.args.filter(_.nonEmpty).flatten
        val compiledArgs = CallArgsCompiler.compileArgs(argTerms, env, fold)(compileTerm)
        def applyFn(fn: Any, values: List[Any]): Any =
          (fn, values) match
            case (f: Function0[?], Nil) =>
              f.asInstanceOf[() => Any].apply()
            case (f: Function1[?, ?], v1 :: Nil) =>
              f.asInstanceOf[Any => Any].apply(v1)
            case (f: Function2[?, ?, ?], v1 :: v2 :: Nil) =>
              f.asInstanceOf[(Any, Any) => Any].apply(v1, v2)
            case (f: Function3[?, ?, ?, ?], v1 :: v2 :: v3 :: Nil) =>
              f.asInstanceOf[(Any, Any, Any) => Any].apply(v1, v2, v3)
            case _ =>
              throw new RuntimeException(s"Unsupported function call: $name/${values.size}")
        Some(
          compiledArgs.flatMap { args =>
            (fnEval, args) match
              case (Eval.Value(fn), evals) if fold && evals.forall(_.isInstanceOf[Eval.Value]) =>
                val values = evals.collect { case Eval.Value(v) => v }
                Right(Eval.Value(applyFn(fn, values)))
              case _ =>
                Right(
                  Eval.BuildList(
                    fnEval :: args,
                    values => applyFn(values.head, values.tail)
                  )
                )
          }
        )
      case _ =>
        None
