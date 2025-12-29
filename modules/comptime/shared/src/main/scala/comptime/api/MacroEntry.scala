package comptime

import scala.quoted.*

private[comptime] object MacroEntry:
  /** Compile-time evaluation macro.
    *
    * All failures become compile errors:
    *   - Cannot compile expression → compile error with reason
    *   - Evaluation throws exception → compile error with message
    *   - Cannot lift result to Expr → compile error
    *
    * No silent runtime fallback.
    */
  def comptimeImpl[A: Type](expr: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*

    // Capture call site info for error reporting
    // Use Position.ofMacroExpansion to get the actual call site, not the inlined code position
    def callSiteInfo: Option[CallSiteInfo] =
      val pos = Position.ofMacroExpansion
      if pos.sourceFile.path == null then None
      else
        try
          val file = pos.sourceFile.path
          val line = pos.startLine + 1
          // Get the original source text from the call site
          val exprText = pos.sourceFile.content match
            case Some(content) =>
              val lines   = content.split('\n')
              val lineIdx = pos.startLine
              if lineIdx >= 0 && lineIdx < lines.length then
                val lineText = lines(lineIdx)
                // Extract just the relevant portion using column positions
                val startCol = pos.startColumn
                val endCol   = math.min(pos.endColumn, lineText.length)
                if startCol < endCol then lineText.substring(startCol, endCol).trim
                else lineText.trim
              else "<unknown>"
            case None => "<unknown>"
          Some(CallSiteInfo(file, line, exprText))
        catch case _: Throwable => None

    // Wrap entire operation - exceptions can occur during:
    // 1. Compilation (constant folding in fold0/fold1/etc)
    // 2. Evaluation (Eval.run)
    // 3. Lifting (ToExpr application)
    try
      val termIR = ScalaAstBridge.termToIR(expr.asTerm)

      Compiler.compileTerm(termIR) match
        case Left(err) =>
          report.errorAndAbort(ComptimeError.format(err))

        case Right(eval) =>
          val value = Eval.run(eval)
          MacroExprs.summonExprOpt[A] match
            case Some(te) =>
              te.asInstanceOf[ToExpr[Any]].apply(value).asExprOf[A]
            case None =>
              val failure = ComptimeError.CannotLift(Type.show[A])
              report.errorAndAbort(ComptimeError.format(failure))
    catch
      case e: ComptimeAbort =>
        val failure = ComptimeError.UserAbort(e.message, callSiteInfo)
        report.errorAndAbort(ComptimeError.format(failure))
      case e: Throwable =>
        val failure = ComptimeError.EvalException(e.getClass.getSimpleName, e.getMessage)
        report.errorAndAbort(ComptimeError.format(failure))
