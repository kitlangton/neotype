package neotype

import scala.quoted.*
import scala.util.{Failure, Success}
import StringFormatting.*

private[neotype] object Macros:
  def applyImpl[A: Type, T: Type, NT <: ValidatedWrapper[A] { type Type = T }: Type](
      a: Expr[A],
      validate: Expr[A => Boolean],
      failureMessage: Expr[String]
  )(using Quotes): Expr[T] =
    import quotes.reflect.*

    lazy val nt = TypeRepr.of[NT].widenTermRefByName match
      case Refinement(t, _, _) => t
      case t                   => t

    lazy val expressionSource: Option[String] =
      validate.asTerm match
        case Uninlined(Block(_, Lambda(_, Seal(Calc(calc))))) =>
          Some(calc.render(using Map("_$1" -> "input".blue)))
        case _ =>
          None

    lazy val treeSource = scala.util
      .Try {
        nt.typeSymbol
          .methodMember("validate")
          .headOption
          .flatMap {
            _.tree match
              case body =>
                body.pos.sourceCode
              case _ =>
                None
          }
      }
      .toOption
      .flatten

    val isBodyInline = nt.typeSymbol
      .methodMember("validate")
      .headOption
      .map(_.flags.is(Flags.Inline))

    a match
      case Calc[A](calc) =>
        scala.util.Try(calc.result(using Map.empty)) match
          case Failure(_) =>
            report.errorAndAbort(ErrorMessages.inputNotKnownAtCompileTime(a, nt))
          case Success(_) =>
            ()
      case _ =>
        report.errorAndAbort(ErrorMessages.inputNotKnownAtCompileTime(a, nt))

    val validateApplied = Expr.betaReduce('{ $validate($a) })
    validateApplied match
      case Calc(calc) =>
        scala.util.Try(calc.result(using Map.empty)) match
          case Failure(exception) =>
            report.errorAndAbort(s"Failed to execute parsed validation: $exception")
          case Success(true) =>
            a.asExprOf[T]
          case Success(false) =>
            val failureMessageValue = failureMessage match
              case Expr(str: String) => str
              case _                 => "Validation Failed"
            report.errorAndAbort(ErrorMessages.validationFailed(a, nt, expressionSource, failureMessageValue))
      case _ =>
        report.errorAndAbort(ErrorMessages.failedToParseValidateMethod(a, nt, treeSource, isBodyInline))

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
