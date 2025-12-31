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
    // Returns (updated env, list of side-effecting evals in reverse order)
    def loop(
        remaining: List[TermIR],
        curEnv: Map[String, Eval],
        sideEffects: List[Eval]
    ): Either[ComptimeError, (Map[String, Eval], List[Eval])] =
      remaining match
        case Nil => Right((curEnv, sideEffects))
        case TermIR.Val(name, value) :: tail =>
          compileTerm(value, curEnv, fold).flatMap { eval =>
            // Determine if this eval has side effects that need to run at definition time
            def hasSideEffects(e: Eval): Boolean = e match
              case Eval.Value(_)           => false
              case Eval.VarBinding(_)      => true // Reading a var is a side effect
              case Eval.ReadRef(_)         => true // Reading a ref is a side effect
              case Eval.WriteRef(_, _)     => true
              case Eval.Seq(_, _)          => true // Seq implies side effects
              case Eval.Apply1(l, r, _)    => hasSideEffects(l) || hasSideEffects(r)
              case Eval.Apply2(a, b, c, _) => hasSideEffects(a) || hasSideEffects(b) || hasSideEffects(c)
              case Eval.Apply3(a, b, c, d, _) =>
                hasSideEffects(a) || hasSideEffects(b) || hasSideEffects(c) || hasSideEffects(d)
              case Eval.ApplyByName1(l, r, _) => hasSideEffects(l) || hasSideEffects(r)
              case Eval.BuildList(elems, _)   => elems.exists(hasSideEffects)
              case Eval.If(c, t, f)           => hasSideEffects(c) || hasSideEffects(t) || hasSideEffects(f)

            eval match
              case Eval.Value(_) =>
                // Constant - no side effects needed, store directly for folding
                loop(tail, curEnv.updated(name, eval), sideEffects)
              case Eval.VarBinding(ref) =>
                // Capture the var's current value into a new cell
                val captureRef    = new MutableRef(null)
                val captureEffect = Eval.WriteRef(captureRef, Eval.ReadRef(ref))
                loop(tail, curEnv.updated(name, Eval.ReadRef(captureRef)), captureEffect :: sideEffects)
              case other if hasSideEffects(other) =>
                // Has side effects - run at declaration time, capture result
                val captureRef    = new MutableRef(null)
                val captureEffect = Eval.WriteRef(captureRef, other)
                loop(tail, curEnv.updated(name, Eval.ReadRef(captureRef)), captureEffect :: sideEffects)
              case other =>
                // Pure computation (like Try.apply) - store directly in environment
                loop(tail, curEnv.updated(name, eval), sideEffects)
          }
        case TermIR.Var(name, value) :: tail =>
          // Create mutable ref, compile init, add write to side effects
          val ref = new MutableRef(null)
          compileTerm(value, curEnv, fold).flatMap { initEval =>
            val writeInit = Eval.WriteRef(ref, initEval)
            loop(tail, curEnv.updated(name, Eval.VarBinding(ref)), writeInit :: sideEffects)
          }
        case other :: tail =>
          compileTerm(other, curEnv, fold).flatMap { eval =>
            loop(tail, curEnv, eval :: sideEffects)
          }

    loop(stats, env, Nil).flatMap { case (newEnv, sideEffects) =>
      compileTerm(expr, newEnv, fold).map { exprEval =>
        // Sequence side effects (in correct order) before the expression
        sideEffects.reverse.foldRight(exprEval) { (effect, acc) =>
          Eval.Seq(effect, acc)
        }
      }
    }
