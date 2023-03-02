package neotype

import scala.quoted.*
import scala.util.{Failure, Success}

private[neotype] object Macros:
  def applyImpl[A: Type, T: Type, NT <: ValidatedWrapper[A] { type Type = T }: Type](
      a: Expr[A],
      validate: Expr[A => Boolean],
      failureMessage: Expr[String]
  )(using Quotes): Expr[T] =
    import quotes.reflect.*

    val failureMessageValue = failureMessage match
      case Expr(str: String) => str
      case _                 => "Validation Failed"

    val nt = TypeRepr.of[NT].widenTermRefByName match
      case Refinement(t, _, _) => t
      case t                   => t

    a match
      case Calc[A](calc) =>
        scala.util.Try(calc.result(using Map.empty)) match
          case Failure(_) =>
            report.errorAndAbort(ErrorMessages.inputNotKnownAtCompileTime(a, nt))
          case Success(_) =>
            ()
      case _ =>
        report.errorAndAbort(ErrorMessages.inputNotKnownAtCompileTime(a, nt))

    def renderedValidate = Expr.betaReduce('{ $validate(INPUT_SENTINEL) }) match
      case whole @ Calc(calc) =>
        calc.render(using Map.empty)
      case other =>
        s"COULD NOT RENDER CALC: ${other.show}"

    val validateApplied = Expr.betaReduce('{ $validate($a) })
    validateApplied match
      case Calc(calc) =>
        scala.util.Try(calc.result(using Map.empty)) match
          case Failure(exception) =>
            report.errorAndAbort(s"Failed to execute parsed validation: $exception")
          case Success(true) =>
            a.asExprOf[T]
          case Success(false) =>
            report.errorAndAbort(ErrorMessages.validationFailed(a, nt, renderedValidate, failureMessageValue))
      case other =>
        val expressionSource = nt.typeSymbol.methodMember("validate").headOption.flatMap(_.tree.pos.sourceCode)
        report.errorAndAbort(ErrorMessages.failedToParseValidateMethod(nt, expressionSource))

  def applyAllImpl[A: Type, T: Type, NT <: Newtype[A] { type Type = T }: Type](
      as: Expr[Seq[A]],
      self: Expr[NT]
  )(using Quotes): Expr[List[T]] =
    import quotes.reflect.*

    def processArgs(args: Seq[Expr[A]]) =
      val applies = args.map { arg =>
        '{ ${ self }.apply(${ arg }) }
      }
      Expr.ofList(applies)

    as match
      case Varargs(args) =>
        processArgs(args)
      case Unseal(Uninlined(Typed(Apply(_, List(Seal(Varargs(args)))), _))) =>
        processArgs(args.asInstanceOf[Seq[Expr[A]]])
      case other =>
        report.errorAndAbort(s"Could not parse input at compile time: ${other.show}")

private inline def INPUT_SENTINEL = ???
