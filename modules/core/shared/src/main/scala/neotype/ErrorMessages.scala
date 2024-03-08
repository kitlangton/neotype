package neotype

import scala.quoted.*
import StringFormatting.*

private[neotype] object ErrorMessages:
  val header =
    "—— Neotype Error ——————————————————————————————————————————————————————————".red
  val footer =
    "———————————————————————————————————————————————————————————————————————————".red

  /** An error message for when the input to a Newtype's apply method is not
    * known at compile time.
    */
  def inputParseFailureMessage(using Quotes)(input: Expr[Any], nt: quotes.reflect.TypeRepr): String =
    import quotes.reflect.*

    val inputTpe = input.asTerm.tpe.widenTermRefByName
    val example  = eval.Eval.renderValue(examples(input))

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
     |  ${"3.".dim} If you are sure the input is valid, use the ${"unsafeMake".green} method:
     |     $newTypeNameString.${"unsafeMake".green}(${valueExprString})
     |  ${"4.".dim} If you think this is a bug, please open an issue at:
     |     ${"https://github.com/kitlangton/neotype/issues".blue.underlined}
     |  $footer
     |""".stripMargin

  /** An error message for when the compile-time validation of a Newtype's apply
    * method fails.
    */
  def validationFailureMessage(using
      Quotes
  )(input: Expr[Any], nt: quotes.reflect.TypeRepr, source: Option[String], maybeCustomMessage: Option[String]): String =
    import quotes.reflect.*

    val customMessage = maybeCustomMessage.map(m => s"\n$m").getOrElse("")

    // indent 2 spaces, make every line bold and red
    val renderedFailure = customMessage.split("\n").map(msg => s"  ${msg.bold}").mkString("\n")
    val sourceExpr      = source.fold("")(s => s"\n  ${"check:".dim} ${s}")

    val inputTpe          = input.asTerm.tpe.widenTermRefByName
    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val valueExprString   = input.asTerm.pos.sourceCode.getOrElse(input.show).blue
    val inputTypeString   = inputTpe.typeSymbol.name.replaceAll("\\$$", "").yellow
    s"""  $header
     |  $newTypeNameString was called with an ${"INVALID".red} $inputTypeString.$renderedFailure
     |  ${"input:".dim} ${valueExprString}$sourceExpr
     |  $footer
     |""".stripMargin

  def validateIsNotInlineMessage(using Quotes)(
      input: Expr[Any], //
      nt: quotes.reflect.TypeRepr,
      validatePosition: Option[quotes.reflect.Position]
  ): String =
    import quotes.reflect.*
    val inputTpe          = input.asTerm.tpe.widenTermRefByName
    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val inputTypeString   = inputTpe.typeSymbol.name.replaceAll("\\$$", "").yellow
    val renderedPosition = validatePosition.fold("") { pos =>
      s"""// ${pos.sourceFile.path}:${pos.startLine}\n  """.stripMargin.dim
    }
    s"""  $header
     |  $newTypeNameString's ${"validate".green} method must be an ${"inline".magenta} def!
     |
     |  ${"Neotype".bold} works by parsing the AST of your ${"validate".green} method into an executable
     |  expression at compile-time. In order to access the AST at compile-time, you
     |  must add the ${"inline".magenta} keyword:
     |
     |  ${renderedPosition}${"override".magenta} ${"inline".magenta.underlined} ${"def".magenta} ${s"validate(input: $inputTypeString".cyan}${"):".cyan} ${"Boolean".yellow} ${"=".cyan} ${"...".cyan}
     |  $footer
     |""".stripMargin

  def failedToParseValidateMethod(using
      Quotes
  )(input: Expr[Any], nt: quotes.reflect.TypeRepr, source: Option[String], missingReference: Option[String]): String =

    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val sourceExpr = source.fold("") { s =>
      s"\n\n${indent(s)}"
    }
    // val inputTpe = input.asTerm.tpe.widenTermRefByName
    // val inputTypeString = inputTpe.typeSymbol.name.replaceAll("\\$$", "").yellow

    val missingReferenceMessage =
      missingReference.fold("") { ref =>
        "\n\n" + s"""  Unknown identifier ${ref.underlined.bold}""".red
      }

    s"""  $header
       |  I've ${"FAILED".red} to parse $newTypeNameString's ${"validate".green} method!$sourceExpr$missingReferenceMessage
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
