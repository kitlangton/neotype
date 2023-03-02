package neotype

import scala.quoted.*
import StringFormatting.*

private[neotype] object ErrorMessages:
  // contiguous ASCII dash symbol: U+2015 looks like: —

  val header =
    "—— Newtype Error ——————————————————————————————————————————————————————————".red
  val footer =
    "———————————————————————————————————————————————————————————————————————————".red

  /** An error message for when the input to a Newtype's apply method is not known at compile time.
    */
  def inputNotKnownAtCompileTime(using Quotes)(input: Expr[Any], nt: quotes.reflect.TypeRepr) =
    import quotes.reflect.*

    val inputTpe = input.asTerm.tpe.widenTermRefByName
    val example  = examples(inputTpe)

    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val valueExprString   = input.asTerm.pos.sourceCode.getOrElse(input.show).blue
    val inputTypeString   = inputTpe.typeSymbol.name.replaceAll("\\$$", "").yellow
    s"""  $header
     |  $newTypeNameString.${"apply".green} requires a compile-time known $inputTypeString.
     |  I could not parse ${valueExprString.underlined}.
     |
     |  🤠 ${"Possible Solutions".bold}
     |  ${"1.".dim} Try passing a literal $inputTypeString:
     |     $newTypeNameString(${example.show.green})
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
  def validationFailed(using
      Quotes
  )(input: Expr[Any], nt: quotes.reflect.TypeRepr, renderedCalc: String, failureMessage: String) =
    import quotes.reflect.*

    val isDefaultFailureMessage = failureMessage == "Validation Failed"
    val renderedFailure         = if isDefaultFailureMessage then "" else s"\n  ${failureMessage.bold}"

    val inputTpe          = input.asTerm.tpe.widenTermRefByName
    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val valueExprString   = input.asTerm.pos.sourceCode.getOrElse(input.show).blue
    val inputTypeString   = inputTpe.typeSymbol.name.replaceAll("\\$$", "").yellow
    s"""  $header
     |  $newTypeNameString was called with an ${"INVALID".red} input $inputTypeString.$renderedFailure
     |  ${"input:".dim} ${valueExprString}
     |  ${"check:".dim} ${renderedCalc}
     |  $footer
     |""".stripMargin

  def failedToParseCustomErrorMessage(using Quotes)(nt: quotes.reflect.TypeRepr) =
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

  def indent(str: String) =
    str.linesIterator
      .map { line =>
        s"  $line".blue
      }
      .mkString("\n")

  def failedToParseValidateMethod(using Quotes)(nt: quotes.reflect.TypeRepr, source: Option[String]) =
    val newTypeNameString = nt.typeSymbol.name.replaceAll("\\$$", "").green.bold
    val sourceExpr = source.fold("") { s =>
      s"\n\n${indent(s)}"
    }
    s"""  $header
     |  I've ${"FAILED".red} to parse $newTypeNameString's ${"validate".green} method!$sourceExpr
     |
     |  ${"Neotype".bold} works by parsing the AST of your ${"validate".green} method into an executable
     |  expression at compile-time. This means I cannot support every possible
     |  Scala expression. However, I'll keeping adding as many as I can!
     |
     |  ${"Possible Solutions".bold}
     |  ${"1.".dim} Make sure validate is an ${"inline".magenta} method.
     |     ${"override inline def validate:".blue} ${"Boolean".yellow} ${"=".blue} ${"n > 0".green}
     |  ${"2.".dim} If you want this expression to be supported, please open an issue at:
     |     ${"https://github.com/kitlangton/neotype/issues".blue.underlined}
     |  $footer
     |""".stripMargin

  // Create a map from various input types to examples of the given type of statically known inputs
  def examples(using Quotes)(tpe: quotes.reflect.TypeRepr) =
    import quotes.reflect.*

    val examples = Map(
      TypeRepr.of[String] -> '{ "foo" },
      TypeRepr.of[Int]    -> '{ 1 },
      TypeRepr.of[Long]   -> '{ 1L },
      TypeRepr.of[Float]  -> '{ 1.0f },
      TypeRepr.of[Double] -> '{ 1.0 }
    )

    examples.find { case (k, _) => k <:< tpe }.get._2
