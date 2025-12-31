package comptime

import neotype.internal.AnsiFormatting.*

/** Context for where a comptime block was called from. */
private[comptime] final case class CallSiteInfo(
    file: String,
    line: Int,
    exprText: String
)

/** Structured compile-time failure types.
  *
  * Each case carries relevant context for error reporting.
  */
enum ComptimeError:
  // ═══════════════════════════════════════════════════════════════════════════
  // Compilation phase failures (converting AST to Eval)
  // ═══════════════════════════════════════════════════════════════════════════

  /** Method/function call not supported by comptime evaluator */
  case UnsupportedCall(owner: String, method: String, details: Map[String, String] = Map.empty)

  /** AST term type not supported */
  case UnsupportedTerm(termKind: String, repr: String)

  /** Identifier could not be resolved */
  case UnresolvedReference(name: String, fullName: Option[String], availableInEnv: List[String])

  /** Wrong number of arguments for operation */
  case UnsupportedArity(operation: String, message: String)

  /** Pattern match failed - no cases matched */
  case MatchError(details: String)

  /** Lambda has too many parameters */
  case UnsupportedLambda(paramCount: Int, maxSupported: Int)

  /** Pattern matching construct not supported */
  case UnsupportedPattern(patternKind: String, details: String)

  // ═══════════════════════════════════════════════════════════════════════════
  // Evaluation phase failures (running the compiled Eval)
  // ═══════════════════════════════════════════════════════════════════════════

  /** Exception thrown during evaluation */
  case EvalException(
      exceptionType: String,
      message: String,
      callContext: Option[String] = None,
      pos: Option[SourcePos] = None
  )

  /** Explicit compile error from user code via comptimeError() */
  case UserAbort(message: String, callSite: Option[CallSiteInfo] = None)

  // ═══════════════════════════════════════════════════════════════════════════
  // Lifting phase failures (converting result back to Expr)
  // ═══════════════════════════════════════════════════════════════════════════

  /** Cannot convert runtime value to compile-time Expr */
  case CannotLift(typeName: String)

object ComptimeError:
  private val header: String =
    "—— Comptime Error ——————————————————————————————————————————————————————————".red
  private val footer: String =
    "————————————————————————————————————————————————————————————————————————————".red

  def format(failure: ComptimeError): String =
    val body = formatBody(failure)
    s"\n$header\n$body\n$footer\n"

  private def formatBody(failure: ComptimeError): String = failure match
    case UnsupportedCall(owner, method, details) =>
      val shortOwner = shortenOwner(owner)
      val lines      = List.newBuilder[String]
      lines += s"  ${"UNSUPPORTED METHOD".red.bold}: ${shortOwner.yellow}.${method.green}"
      lines += ""
      lines += "  The compile-time evaluator doesn't support this method yet."
      if details.nonEmpty then
        details.toList.sortBy(_._1).foreach { case (k, v) =>
          lines += s"  $k: $v".dim
        }
      lines += ""
      lines += s"  ${"Report".dim}: ${"https://github.com/kitlangton/neotype/issues".blue.underlined}"
      lines.result().mkString("\n")

    case UnsupportedTerm(kind, repr) =>
      val shortRepr = if repr.length > 60 then repr.take(57) + "..." else repr
      val lines     = List.newBuilder[String]
      lines += s"  ${"UNSUPPORTED SYNTAX".red.bold}: ${kind.yellow}"
      lines += ""
      lines += "  This Scala construct can't be evaluated at compile-time:"
      lines += s"  ${shortRepr.dim}"
      lines += ""
      lines += s"  ${"Tip".cyan}: Simplify the expression or move it outside the comptime block."
      lines.result().mkString("\n")

    case UnresolvedReference(name, fullName, available) =>
      val lines = List.newBuilder[String]
      lines += s"  ${"UNRESOLVED REFERENCE".red.bold}: ${name.yellow}"
      fullName.foreach(fn => lines += s"  ${"Full name".dim}: $fn")
      lines += ""
      lines += "  This identifier isn't available during compile-time evaluation."
      lines += ""
      lines += s"  ${"Note".cyan}: comptime can only access:"
      lines += "    • Literals and inline values"
      lines += "    • Local vals defined in the comptime block"
      lines += "    • Methods on standard library types"
      if available.nonEmpty then
        val shown  = available.take(8).map(_.green).mkString(", ")
        val suffix = if available.size > 8 then ", ..." else ""
        lines += ""
        lines += s"  ${"Available in scope".dim}: $shown$suffix"
      lines.result().mkString("\n")

    case UnsupportedArity(op, msg) =>
      val lines = List.newBuilder[String]
      if msg.nonEmpty then lines += s"  ${"UNSUPPORTED ARITY".red.bold}: ${op.yellow}: $msg"
      else lines += s"  ${"UNSUPPORTED ARITY".red.bold}: ${op.yellow}"
      lines += ""
      lines += "  This operation was called with an unsupported number of arguments."
      lines.result().mkString("\n")

    case MatchError(details) =>
      val lines = List.newBuilder[String]
      lines += s"  ${"MATCH ERROR".red.bold}"
      lines += ""
      lines += s"  Pattern match failed: ${details.yellow}"
      lines += ""
      lines += s"  ${"Tip".cyan}: Ensure all possible cases are covered."
      lines.result().mkString("\n")

    case UnsupportedLambda(count, max) =>
      val lines = List.newBuilder[String]
      lines += s"  ${"UNSUPPORTED LAMBDA".red.bold}"
      lines += ""
      lines += s"  Lambda with ${count.toString.yellow} parameters not supported."
      lines += s"  Maximum supported: ${max.toString.green} parameters."
      lines += ""
      lines += s"  ${"Tip".cyan}: Break into nested single-parameter lambdas if possible."
      lines.result().mkString("\n")

    case UnsupportedPattern(kind, details) =>
      val lines = List.newBuilder[String]
      lines += s"  ${"UNSUPPORTED PATTERN".red.bold}: ${kind.yellow}"
      lines += ""
      lines += "  This pattern matching construct isn't supported yet:"
      lines += s"  ${details.dim}"
      lines += ""
      lines += s"  ${"Supported patterns".cyan}: literals, wildcards, typed, Option, Either,"
      lines += "  Tuple, List, Vector, Array, case classes"
      lines.result().mkString("\n")

    case EvalException(exType, msg, callContext, pos) =>
      val msgStr = if msg != null && msg.nonEmpty then msg else "(no message)"
      val lines  = List.newBuilder[String]
      lines += s"  ${exType.yellow}: ${msgStr.white}"
      // Show source with context lines (dimmed) and failing part highlighted in red
      pos.foreach { p =>
        if p.lineContent.nonEmpty then
          lines += ""
          // Calculate gutter width for line numbers
          val allLineNums = p.contextBefore.map(_._1) ++ List(p.line) ++ p.contextAfter.map(_._1)
          val gutterWidth = allLineNums.map(_.toString.length).maxOption.getOrElse(1)
          def gutter(lineNum: Int): String =
            val numStr = lineNum.toString.reverse.padTo(gutterWidth, ' ').reverse
            s"  $numStr │ "
          // Context before (dimmed)
          p.contextBefore.foreach { case (num, content) =>
            lines += s"${gutter(num)}$content".dim
          }
          // Main line: bright, with failing part in red
          val before  = p.lineContent.take(p.startCol)
          val failing = p.lineContent.slice(p.startCol, p.endCol)
          val after   = p.lineContent.drop(p.endCol)
          lines += s"${gutter(p.line).white}${before.white}${failing.red}${after.white}"
          // Context after (dimmed)
          p.contextAfter.foreach { case (num, content) =>
            lines += s"${gutter(num)}$content".dim
          }
      }
      callContext.foreach { ctx =>
        lines += ""
        lines += s"  ${"in".dim} ${ctx.cyan}"
      }
      lines.result().mkString("\n")

    case UserAbort(msg, callSite) =>
      val lines = List.newBuilder[String]
      lines += s"  ${msg.white}"
      callSite.foreach { site =>
        val shortFile = site.file.split('/').lastOption.getOrElse(site.file)
        lines += ""
        lines += s"  ${"at".dim} ${shortFile.cyan}:${site.line.toString.yellow}"
        lines += s"     ${site.exprText.dim}"
      }
      lines.result().mkString("\n")

    case CannotLift(typeName) =>
      val lines = List.newBuilder[String]
      lines += s"  ${"CANNOT LIFT RESULT".red.bold}"
      lines += ""
      lines += "  The compile-time result can't be converted back to code."
      lines += s"  ${"Type".dim}: ${typeName.yellow}"
      lines += ""
      lines += "  This usually happens with:"
      lines += s"    • Union types (${"A | B".cyan})"
      lines += s"    • Types without a ${"ToExpr".green} instance"
      lines += "    • Complex runtime-only types"
      lines += ""
      lines += s"  ${"Tip".cyan}: Return a concrete type like ${"String".green}, ${"Int".green}, or a case class."
      lines.result().mkString("\n")

  private def shortenOwner(owner: String): String =
    owner.split('.').lastOption.getOrElse(owner).stripSuffix("$")

private[comptime] object ComptimeDebug:
  inline val enabled = false
  inline def log(msg: => String): Unit =
    if enabled then println(msg)
