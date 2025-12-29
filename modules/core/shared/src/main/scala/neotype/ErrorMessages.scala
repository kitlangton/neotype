package neotype

import neotype.internal.AnsiFormatting.*

import scala.quoted.*
import scala.util.matching.Regex

/** Context extracted from macro expansion for error messages */
private[neotype] case class MessageContext(
    newtypeName: String,
    inputExpr: String,
    inputType: String
)

private[neotype] object MessageContext:
  def from(using Quotes)(input: Expr[Any], nt: quotes.reflect.TypeRepr): MessageContext =
    import quotes.reflect.*
    val inputTpe = input.asTerm.tpe.widenTermRefByName
    MessageContext(
      newtypeName = nt.typeSymbol.name.replaceAll("\\$$", ""),
      inputExpr = input.asTerm.pos.sourceCode.getOrElse(input.show),
      inputType = inputTpe.typeSymbol.name.replaceAll("\\$$", "")
    )

/** Structured error types for neotype compile-time errors */
private[neotype] sealed trait NeotypeError:
  def render: String

private[neotype] object NeotypeError:
  val header: String =
    "—— Neotype Error ——————————————————————————————————————————————————————————".red
  val footer: String =
    "———————————————————————————————————————————————————————————————————————————".red

  case class InputParseFailure(
      ctx: MessageContext,
      example: String
  ) extends NeotypeError:
    def render: String =
      val nt   = ctx.newtypeName.green.bold
      val expr = ctx.inputExpr.blue
      val tpe  = ctx.inputType.yellow
      s"""  $header
         |  $nt.${"apply".green} requires a compile-time known $tpe.
         |  I could not parse ${expr.underlined}.
         |
         |  🤠 ${"Possible Solutions".bold}
         |  ${"1.".dim} Try passing a literal $tpe:
         |     $nt($example)
         |  ${"2.".dim} Call the ${"make".green} method, which returns a runtime-validated ${"Either".yellow}:
         |     $nt.${"make".green}($expr)
         |  ${"3.".dim} If you are sure the input is valid, use the ${"unsafeMake".green} method:
         |     $nt.${"unsafeMake".green}($expr)
         |  ${"4.".dim} If you think this is a bug, please open an issue at:
         |     ${"https://github.com/kitlangton/neotype/issues".blue.underlined}
         |  $footer
         |""".stripMargin

  case class ValidationFailed(
      ctx: MessageContext,
      source: Option[String],
      customMessage: Option[String]
  ) extends NeotypeError:
    def render: String =
      val nt          = ctx.newtypeName.green.bold
      val expr        = ctx.inputExpr.blue
      val tpe         = ctx.inputType.yellow
      val msgRendered = customMessage.map(m => s"\n$m").getOrElse("")
      val failure     = msgRendered.split("\n").map(msg => s"  ${msg.bold}").mkString("\n")
      val sourceExpr  = source.fold("")(s => s"\n  ${"check:".dim} $s")
      s"""  $header
         |  $nt was called with an ${"INVALID".red} $tpe.$failure
         |  ${"input:".dim} $expr$sourceExpr
         |  $footer
         |""".stripMargin

  case class ValidateNotInline(
      ctx: MessageContext,
      position: Option[String]
  ) extends NeotypeError:
    def render: String =
      val nt               = ctx.newtypeName.green.bold
      val tpe              = ctx.inputType.yellow
      val renderedPosition = position.fold("")(p => s"$p\n  ".dim)
      s"""  $header
         |  $nt's ${"validate".green} method must be an ${"inline".magenta} def!
         |
         |  ${"Neotype".bold} works by parsing the AST of your ${"validate".green} method into an executable
         |  expression at compile-time. In order to access the AST at compile-time, you
         |  must add the ${"inline".magenta} keyword:
         |
         |  $renderedPosition${"override".magenta} ${"inline".magenta.underlined} ${"def".magenta} ${s"validate(input: $tpe".cyan}${"):".cyan} ${"Boolean".yellow} ${"=".cyan} ${"...".cyan}
         |  $footer
         |""".stripMargin

  case class ParseMethodFailed(
      ctx: MessageContext,
      source: Option[String],
      missingReference: Option[String],
      details: Option[String]
  ) extends NeotypeError:
    def render: String =
      val nt        = ctx.newtypeName.green.bold
      val sourceMsg = source.fold("")(s => s"\n\n${indent(s)}")
      val refMsg = missingReference.fold("") { ref =>
        "\n\n" + s"  Unknown identifier ${ref.underlined.bold}".red
      }
      val detailsMsg = details.fold("") { d =>
        "\n\n" + s"""  ${"Details:".dim}
                    |${indent(d)}""".stripMargin
      }
      s"""  $header
         |  I've ${"FAILED".red} to parse $nt's ${"validate".green} method!$sourceMsg$refMsg$detailsMsg
         |
         |  ${"Neotype".bold} works by parsing the AST of your ${"validate".green} method into an executable
         |  expression at compile-time. Sadly, this means I cannot parse arbitrary,
         |  user-defined methods. Likewise, support for the entire Scala standard
         |  library is incomplete.
         |
         |  If you think this expression should be supported, please open an issue at:
         |  ${"https://github.com/kitlangton/neotype/issues".blue.underlined}
         |  $footer
         |""".stripMargin

  private def indent(str: String): String =
    str.linesIterator.map(line => s"  $line".blue).mkString("\n")

/** Helper methods for backward compatibility with Macros.scala */
private[neotype] object ErrorMessages:
  def inputParseFailureMessage(using Quotes)(input: Expr[Any], nt: quotes.reflect.TypeRepr): String =
    val ctx     = MessageContext.from(input, nt)
    val example = renderValue(examples(input))
    NeotypeError.InputParseFailure(ctx, example).render

  def validationFailureMessage(using
      Quotes
  )(
      input: Expr[Any],
      nt: quotes.reflect.TypeRepr,
      source: Option[String],
      maybeCustomMessage: Option[String]
  ): String =
    val ctx = MessageContext.from(input, nt)
    NeotypeError.ValidationFailed(ctx, source, maybeCustomMessage).render

  def validateIsNotInlineMessage(using
      Quotes
  )(
      input: Expr[Any],
      nt: quotes.reflect.TypeRepr,
      validatePosition: Option[quotes.reflect.Position]
  ): String =
    val ctx = MessageContext.from(input, nt)
    val posStr = validatePosition.map { pos =>
      s"// ${pos.sourceFile.path}:${pos.startLine}"
    }
    NeotypeError.ValidateNotInline(ctx, posStr).render

  def failedToParseValidateMethod(using
      Quotes
  )(
      input: Expr[Any],
      nt: quotes.reflect.TypeRepr,
      source: Option[String],
      missingReference: Option[String],
      details: Option[String]
  ): String =
    val ctx = MessageContext.from(input, nt)
    NeotypeError.ParseMethodFailed(ctx, source, missingReference, details).render

  def parseInputNotConstantMessage(using Quotes)(input: Expr[Any], parserName: String): String =
    import quotes.reflect.*
    val inputExpr = input.asTerm.pos.sourceCode.getOrElse(input.show)
    val inputTpe  = input.asTerm.tpe.widenTermRefByName.typeSymbol.name.replaceAll("\\$$", "")
    val example   = renderValue(examples(input))
    val parser    = parserName.green.bold
    val expr      = inputExpr.blue
    val tpe       = inputTpe.yellow
    s"""  ${NeotypeError.header}
       |  $parser.${"apply".green} requires a compile-time known $tpe.
       |  I could not parse ${expr.underlined}.
       |
       |  🤠 ${"Possible Solutions".bold}
       |  ${"1.".dim} Try passing a literal $tpe:
       |     $parser($example)
       |  ${"2.".dim} Call the ${"make".green} method, which returns a runtime-validated ${"Either".yellow}:
       |     $parser.${"make".green}($expr)
       |  ${NeotypeError.footer}
       |""".stripMargin

  def parseFailureMessage(using Quotes)(input: Expr[Any], parserName: String, message: String): String =
    import quotes.reflect.*
    val inputExpr = input.asTerm.pos.sourceCode.getOrElse(input.show)
    val parser    = parserName.green.bold
    val expr      = inputExpr.blue
    val msg       = message.bold
    s"""  ${NeotypeError.header}
       |  $parser failed to parse input.
       |  ${"input:".dim} $expr
       |  ${"error:".dim} $msg
       |  ${NeotypeError.footer}
       |""".stripMargin

  def parseNotInlineMessage(using
      Quotes
  )(
      input: Expr[Any],
      parserName: String,
      parsePosition: Option[quotes.reflect.Position]
  ): String =
    import quotes.reflect.*
    val inputTpe    = input.asTerm.tpe.widenTermRefByName.typeSymbol.name.replaceAll("\\$$", "")
    val parser      = parserName.green.bold
    val tpe         = inputTpe.yellow
    val renderedPos = parsePosition.map(pos => s"// ${pos.sourceFile.path}:${pos.startLine}")
    val posStr      = renderedPos.fold("")(p => s"$p\n  ".dim)
    s"""  ${NeotypeError.header}
       |  $parser's ${"parse".green} method must be an ${"inline".magenta} def!
       |
       |  ${"Neotype".bold} works by parsing the AST of your ${"parse".green} method into an executable
       |  expression at compile-time. In order to access the AST at compile-time, you
       |  must add the ${"inline".magenta} keyword:
       |
       |  $posStr${"override".magenta} ${"inline".magenta.underlined} ${"def".magenta} ${s"parse(input: $tpe".cyan}${"): Either[String, ...]".cyan}
       |  ${NeotypeError.footer}
       |""".stripMargin

  def parseMethodFailedMessage(
      parserName: String,
      details: Option[String]
  ): String =
    val parser = parserName.green.bold
    val detailsMsg = details.fold("")(d => "\n\n" + s"""  ${"Details:".dim}
                      |${indent(d)}""".stripMargin)
    s"""  ${NeotypeError.header}
       |  I've ${"FAILED".red} to parse $parser's ${"parse".green} method!$detailsMsg
       |
       |  ${"Neotype".bold} works by parsing the AST of your ${"parse".green} method into an executable
       |  expression at compile-time. Sadly, this means I cannot parse arbitrary,
       |  user-defined methods. Likewise, support for the entire Scala standard
       |  library is incomplete.
       |
       |  If you think this expression should be supported, please open an issue at:
       |  ${"https://github.com/kitlangton/neotype/issues".blue.underlined}
       |  ${NeotypeError.footer}
       |""".stripMargin

  private def renderValue(value: Any): String =
    value match
      case s: String => s""""$s"""".green
      case c: Char   => s"'$c'".green
      case set: Set[?] =>
        val elems = set.map(renderValue).mkString(", ".reset)
        "Set(".reset + elems + ")".reset
      case list: List[?] =>
        val elems = list.map(renderValue).mkString(", ".reset)
        "List(".reset + elems + ")".reset
      case regex: Regex =>
        s"""${renderValue(regex.toString)}.r"""
      case long: Long => s"${long}L".cyan
      case _          => value.toString.cyan

  private def indent(str: String): String =
    str.linesIterator.map(line => s"  $line".blue).mkString("\n")

  def examples(using Quotes)(input: Expr[Any]): Any =
    input match
      case '{ ($_): String }  => "foo"
      case '{ ($_): Int }     => 1
      case '{ ($_): Long }    => 1L
      case '{ ($_): Float }   => 1.0f
      case '{ ($_): Double }  => 1.0
      case '{ ($_): Boolean } => true
      case '{ ($_): Char }    => 'a'
      case '{ ($_): Byte }    => 1.toByte
      case '{ ($_): Short }   => 1.toShort
      case '{ ($_): Unit }    => ()
      case '{ ($_): Null }    => null
      case _                  => "input"
