package comptime

private[comptime] object TermCompiler:
  // Common exception types that can be reconstructed from EvalException
  private val exceptionConstructors: Map[String, String => Throwable] = Map(
    "NumberFormatException"           -> (msg => new NumberFormatException(msg)),
    "ArithmeticException"             -> (msg => new ArithmeticException(msg)),
    "IllegalArgumentException"        -> (msg => new IllegalArgumentException(msg)),
    "IllegalStateException"           -> (msg => new IllegalStateException(msg)),
    "NullPointerException"            -> (msg => new NullPointerException(msg)),
    "IndexOutOfBoundsException"       -> (msg => new IndexOutOfBoundsException(msg)),
    "StringIndexOutOfBoundsException" -> (msg => new StringIndexOutOfBoundsException(msg)),
    "ArrayIndexOutOfBoundsException"  -> (msg => new ArrayIndexOutOfBoundsException(msg)),
    "NoSuchElementException"          -> (msg => new NoSuchElementException(msg)),
    "UnsupportedOperationException"   -> (msg => new UnsupportedOperationException(msg)),
    "RuntimeException"                -> (msg => new RuntimeException(msg))
  )

  private def reconstructException(exType: String, msg: String): Option[Throwable] =
    exceptionConstructors.get(exType).map(ctor => ctor(msg))

  def compileTerm(
      term: TermIR,
      env: Map[String, Eval],
      fold: Boolean,
      compileCall: (CallIR, RuleContext) => Either[ComptimeError, Eval]
  ): Either[ComptimeError, Eval] =
    def loop(term: TermIR, env: Map[String, Eval], fold: Boolean): Either[ComptimeError, Eval] =
      val ctx = RuleContext(
        foldConstants = fold,
        compileTerm = t => loop(t, env, fold),
        compileTermLazy = t => loop(t, env, fold = false)
      )
      term match
        case TermIR.Lit(value) => Right(Eval.Value(value))
        case TermIR.Ref(name, fullName) =>
          ReferenceResolver.resolveOrError(name, fullName, env)
        case TermIR.CaseClass(fullName, fields, repeatedIndex, args) =>
          val compiledArgsE = CallArgsCompiler.compileArgs(args, env, fold)(loop)
          compiledArgsE.map { evals =>
            val values = evals.collect { case Eval.Value(v) => v }
            if fold && values.size == evals.size then
              Eval.Value(CaseClassBuilder.build(fullName, fields, repeatedIndex, values))
            else Eval.BuildList(evals, args => CaseClassBuilder.build(fullName, fields, repeatedIndex, args))
          }
        case TermIR.Lambda(params, body) =>
          LambdaCompiler.compileLambda(params, body, env)(
            loop,
            (value, cases, env2, fold2) => MatchCompiler.compileMatch(value, cases, env2, fold2)(loop)
          )
        case TermIR.Call(call) =>
          FunctionCallCompiler.compileFunctionCall(call, env, fold)(loop).getOrElse(compileCall(call, ctx))
        case TermIR.If(cond, onTrue, onFalse) =>
          ctx.compileTerm(cond).flatMap {
            case Eval.Value(value: Boolean) if ctx.foldConstants =>
              if value then loop(onTrue, env, fold) else loop(onFalse, env, fold)
            case condEval =>
              for
                trueEval  <- ctx.compileTermLazy(onTrue)
                falseEval <- ctx.compileTermLazy(onFalse)
              yield Eval.If(condEval, trueEval, falseEval)
          }
        case TermIR.Match(scrutinee, cases) =>
          ctx.compileTerm(scrutinee).flatMap { scrutEval =>
            val scrutValue = Eval.run(scrutEval)
            MatchCompiler.compileMatch(scrutValue, cases, env, fold)(loop)
          }
        case TermIR.Block(stats, expr) =>
          BlockCompiler.compileBlock(stats, expr, env, fold)(loop)
        case TermIR.Throw(expr) =>
          // Compile and evaluate the exception, then throw it
          loop(expr, env, fold).flatMap { exprEval =>
            val exc = Eval.run(exprEval)
            exc match
              case t: Throwable => throw t
              case other        => throw new RuntimeException(s"Throw requires Throwable, got: ${other.getClass}")
          }
        case TermIR.Try(expr, cases, finalizer) =>
          // Helper to convert exception to EvalException
          def toEvalException(e: Throwable): ComptimeError.EvalException =
            val exType = e.getClass.getSimpleName
            val msg    = if e.getMessage != null then e.getMessage else ""
            ComptimeError.EvalException(exType, msg, None, None)

          // Run the finalizer - throws if finally block throws
          def runFinally(): Unit =
            finalizer.foreach { fin =>
              loop(fin, env, fold) match
                case Right(finEval) =>
                  Eval.run(finEval) // May throw - that's intentional
                case Left(ComptimeError.EvalException(exType, msg, _, _)) =>
                  // Finally compilation threw - reconstruct and throw
                  reconstructException(exType, msg).foreach(throw _)
                case Left(_) =>
                  () // Ignore other compile errors in finally (e.g., UnsupportedCall)
            }

          // Wrap result computation with finally, handling finally exceptions
          def withFinally[A](compute: => Either[ComptimeError, A]): Either[ComptimeError, A] =
            try
              val result = compute
              runFinally()
              result
            catch
              case e: Throwable =>
                // finally threw - its exception replaces any prior result
                Left(toEvalException(e))

          def handleException(e: Throwable): Either[ComptimeError, Eval] =
            if ComptimeDebug.enabled then
              ComptimeDebug.log(s"[try/catch] handling exception: ${e.getClass.getName}: ${e.getMessage}")
            // Wrap in try/catch to handle throws that occur during catch body compilation
            // (e.g., `throw e` in a catch clause executes during compilation, not Eval.run)
            try
              MatchCompiler.compileMatch(e, cases, env, fold)(loop) match
                case Right(eval) =>
                  // Catch matched - run handler with finally protection
                  withFinally {
                    try Right(Eval.Value(Eval.run(eval)))
                    catch
                      case handlerEx: Throwable =>
                        // Handler threw - propagate handler's exception (not original)
                        Left(toEvalException(handlerEx))
                  }
                case Left(ComptimeError.MatchError(_)) =>
                  // No catch case matched - run finally, propagate original exception
                  withFinally(Left(toEvalException(e)))
                case Left(other) =>
                  // Pattern/guard error - run finally, propagate that error
                  withFinally(Left(other))
            catch
              case compileThrow: Throwable =>
                // Catch body threw during compilation (e.g., explicit `throw e`)
                withFinally(Left(toEvalException(compileThrow)))

          // Try to compile and run the body
          loop(expr, env, fold) match
            case Right(bodyEval) =>
              try
                val result = Eval.run(bodyEval)
                // Body succeeded - run finally with protection
                withFinally(Right(Eval.Value(result)))
              catch case e: Throwable => handleException(e)
            case Left(ComptimeError.EvalException(exType, msg, _, _)) =>
              // Exception during compilation - try to reconstruct and catch
              if ComptimeDebug.enabled then ComptimeDebug.log(s"[try/catch] reconstructing: $exType")
              reconstructException(exType, msg) match
                case Some(e) => handleException(e)
                case None    => withFinally(Left(ComptimeError.EvalException(exType, msg, None, None)))
            case Left(other) =>
              withFinally(Left(other))
        case other => Left(ComptimeError.UnsupportedTerm(other.getClass.getSimpleName, other.toString))

    loop(term, env, fold)
