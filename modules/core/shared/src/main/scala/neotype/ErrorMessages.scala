package neotype

import scala.quoted.*
import StringFormatting.*

private[neotype] object ErrorMessages:
  val header =
    "—— Newtype Error ——————————————————————————————————————————————————————————".red
  val footer =
    "———————————————————————————————————————————————————————————————————————————".red

  /** An error message for when the input to a Newtype's apply method is not known at compile time.
    */
  def inputParseFailureMessage(using Quotes)(input: Expr[Any], nt: quotes.reflect.TypeRepr): String =
    import quotes.reflect.*

    val inputTpe = input.asTerm.tpe.widenTermRefByName
    val example  = Calc.renderValue(examples(input))

    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val valueExprString   = input.asTerm.pos.sourceCode.getOrElse(input.show).blue
    val inputTypeString   = inputTpe.typeSymbol.name.replaceAll("\\$$", "").yellow
    s"""  $header
     |  $newTypeNameString.${"apply".green} requires a compile-time known $inputTypeString.
     |  I could not parse ${valueExprString.underlined}.
     |
     |  🤠 ${"Possible Solutions".bold}
     |  ${"1.".dim} Try passing a literal $inputTypeString:
     |     $newTypeNameString(${example})
     |  ${"2.".dim} Call the ${"make".green} method, which returns a runtime-validated ${"Either".yellow}:
     |     $newTypeNameString.${"make".green}(${valueExprString})
     |  ${"3.".dim} If you are sure the input is valid, use the ${"unsafe".green} method:
     |     $newTypeNameString.${"unsafe".green}(${valueExprString})
     |  ${"4.".dim} If you think this is a bug, please open an issue at:
     |     ${"https://github.com/kitlangton/neotype/issues".blue.underlined}
     |  $footer
     |""".stripMargin

  /** An error message for when the compile-time validation of a Newtype's apply method fails.
    */
  def compileTimeValidationFailureMessage(using
      Quotes
  )(input: Expr[Any], nt: quotes.reflect.TypeRepr, source: Option[String], failureMessage: String): String =
    import quotes.reflect.*

    val isDefaultFailureMessage = failureMessage == "Validation Failed"
    val renderedFailure         = if isDefaultFailureMessage then "" else s"\n  ${failureMessage.bold}"
    val sourceExpr = source.fold("") { s =>
      s"\n  ${"check:".dim} ${s}"
    }

    val inputTpe          = input.asTerm.tpe.widenTermRefByName
    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val valueExprString   = input.asTerm.pos.sourceCode.getOrElse(input.show).blue
    val inputTypeString   = inputTpe.typeSymbol.name.replaceAll("\\$$", "").yellow
    s"""  $header
     |  $newTypeNameString was called with an ${"INVALID".red} $inputTypeString.$renderedFailure
     |  ${"input:".dim} ${valueExprString}$sourceExpr
     |  $footer
     |""".stripMargin

  def validateIsNotInlineMessage(using Quotes)(input: Expr[Any], nt: quotes.reflect.TypeRepr): String =
    import quotes.reflect.*
    val inputTpe          = input.asTerm.tpe.widenTermRefByName
    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val inputTypeString   = inputTpe.typeSymbol.name.replaceAll("\\$$", "").yellow
    s"""  $header
     |  $newTypeNameString's ${"validate".green} method must be an ${"inline".magenta} def!
     |
     |  ${"Neotype".bold} works by parsing the AST of your ${"validate".green} method into an executable
     |  expression at compile-time. In order to access the AST at compile-time, you
     |  must add the ${"inline".magenta} keyword:
     |
     |  ${"inline".magenta.underlined} ${"def".magenta} ${s"validate(input: $inputTypeString".cyan}${"):".cyan} ${"Boolean".yellow} ${"=".cyan} ${"...".cyan}
     |  $footer
     |""".stripMargin

  def failedToParseCustomErrorMessage(using Quotes)(nt: quotes.reflect.TypeRepr): String =
    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    s"""  $header
     |  😭 I've ${"FAILED".red} to parse $newTypeNameString's ${"failureMessage".green}!
     |
     |  ${"Possible Solutions".bold}
     |  ${"1.".dim} Make sure your failure message is an ${"inline".magenta} ${"String".yellow} literal.
     |     ${"override inline def failureMessage:".blue} ${"String".yellow} ${"=".blue} ${"\"Expected a fibonacci number...\"".green}
     |  ${"2.".dim} If you think this is a bug, please open an issue at:
     |     ${"https://github.com/kitlangton/neotype/issues".blue.underlined}
     |  $footer
     |""".stripMargin

  def failedToParseValidateMethod(using
      Quotes
  )(input: Expr[Any], nt: quotes.reflect.TypeRepr, source: Option[String], isBodyInline: Boolean): String =
    import quotes.reflect.*
    if !isBodyInline then return validateIsNotInlineMessage(input, nt)

    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val sourceExpr = source.fold("") { s =>
      s"\n\n${indent(s)}"
    }
    val inputTpe        = input.asTerm.tpe.widenTermRefByName
    val inputTypeString = inputTpe.typeSymbol.name.replaceAll("\\$$", "").yellow
    val solutionMessage =
      // TODO: Eliminate the duplication here. Can remove else case?
      if isBodyInline then s"""
          |  💁 If you want this expression to be supported, please open an issue at:
          |     ${"https://github.com/kitlangton/neotype/issues".blue.underlined}""".stripMargin
      else s"""
        |  ${"Possible Solutions".bold}
        |  ${"1.".dim} Make sure validate is an ${"inline".magenta} method.
        |     ${"inline".magenta.underlined} ${"def".magenta} ${s"validate(input: $inputTypeString".blue}${"):".blue} ${"Boolean".yellow} ${"=".blue} ${"...".blue}
        |  ${"2.".dim} If you want this expression to be supported, please open an issue at:
        |     ${"https://github.com/kitlangton/neotype/issues".blue.underlined}""".stripMargin
    s"""  $header
       |  I've ${"FAILED".red} to parse $newTypeNameString's ${"validate".green} method!$sourceExpr
       |
       |  ${"Neotype".bold} works by parsing the AST of your ${"validate".green} method into an executable
       |  expression at compile-time. This means I cannot support every possible
       |  Scala expression. However, I'll keeping adding as many as I can!
       |  $solutionMessage
       |  $footer
       |""".stripMargin

  private def indent(str: String) =
    str.linesIterator
      .map { line =>
        s"  $line".blue
      }
      .mkString("\n")

  // Create a map from various input types to examples of the given type of statically known inputs
  def examples(using Quotes)(input: Expr[Any]): Any =
    import quotes.reflect.*

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
