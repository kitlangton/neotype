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
          Some(calc.render(using Map("INPUT" -> "input".blue)))
        case _ =>
          None

    lazy val treeSource =
      try
        nt.typeSymbol
          .methodMember("validate")
          .headOption
          .flatMap {
            _.tree.pos.sourceCode
          }
      catch case _: Throwable => None

    val isBodyInline = nt.typeSymbol
      .methodMember("validate")
      .headOption
      .map(_.flags.is(Flags.Inline))

    a match
      case Calc[A](calc) =>
        scala.util.Try(calc.result(using Map.empty)) match
          case Failure(_) =>
            report.errorAndAbort(ErrorMessages.inputParseFailureMessage(a, nt))
          case Success(_) =>
            ()
      case _ =>
        report.errorAndAbort(ErrorMessages.inputParseFailureMessage(a, nt))

    val validateApplied = Expr.betaReduce('{ $validate($a) })
    validateApplied match
      case Calc[A](calc) =>
        scala.util.Try(calc.result(using Map.empty)) match
          case Failure(exception) =>
//            report.errorAndAbort(s"Failed to execute parsed validation: $exception")
            report.errorAndAbort(ErrorMessages.failedToParseValidateMethod(a, nt, treeSource, isBodyInline))
          case Success(true) =>
            a.asExprOf[T]
          case Success(false) =>
            val failureMessageValue = failureMessage match
              case Expr(str: String) => str
              case _                 => "Validation Failed"
            report.errorAndAbort(
              ErrorMessages.compileTimeValidationFailureMessage(a, nt, expressionSource, failureMessageValue)
            )
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

private[neotype] object TestMacros:
  inline def eval[A](inline expr: A): A      = ${ evalImpl[A]('expr) }
  inline def evalDebug[A](inline expr: A): A = ${ evalDebugImpl[A]('expr) }

  def evalDebugImpl[A: Type](using Quotes)(expr: Expr[A]): Expr[A] =
    import quotes.reflect.*
    report.info(s"expr: ${expr.show}\nterm: ${expr.asTerm.underlyingArgument}")
    evalImpl(expr)

  def evalImpl[A: Type](using Quotes)(expr: Expr[A]): Expr[A] =
    import quotes.reflect.*
    expr match
      case Calc[A](calc) =>
        val result      = calc.result(using Map.empty)
        given ToExpr[A] = toExprInstance(result).asInstanceOf[ToExpr[A]]
        Expr(result)
      case _ =>
        report.errorAndAbort(s"Could not parse input at compile time: ${expr.show}\n\n${expr.asTerm.toString.blue}")
        ???

  def toExprInstance(using Quotes)(any: Any): ToExpr[?] =
    import quotes.reflect.*
    any match
      case _: Int        => summon[ToExpr[Int]]
      case _: String     => summon[ToExpr[String]]
      case _: Boolean    => summon[ToExpr[Boolean]]
      case _: Long       => summon[ToExpr[Long]]
      case _: Double     => summon[ToExpr[Double]]
      case _: Float      => summon[ToExpr[Float]]
      case _: Char       => summon[ToExpr[Char]]
      case _: Byte       => summon[ToExpr[Byte]]
      case _: Short      => summon[ToExpr[Short]]
      case _: Set[Int]   => summon[ToExpr[Set[Int]]]
      case _: List[Int]  => summon[ToExpr[List[Int]]]
      case _: BigInt     => summon[ToExpr[BigInt]]
      case _: BigDecimal => summon[ToExpr[BigDecimal]]
