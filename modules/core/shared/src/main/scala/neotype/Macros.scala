package neotype

import scala.quoted.*
import scala.util.{Failure, Success}
import StringFormatting.*
import neotype.eval.{Unseal, Eval, Seal, Uninlined, EvalError}

private[neotype] object Macros:

  def applyImpl[Input: Type, T: Type, NT <: Neotype[Input] { type Type = T }: Type](
      inputExpr: Expr[Input],
      validate: Expr[Input => Boolean | String]
  )(using Quotes): Expr[T] =
    import quotes.reflect.*

    lazy val nt = TypeRepr.of[NT].widenTermRefByName match
      case Refinement(t, _, _) => t
      case t                   => t

    val validateMethod = nt.typeSymbol.declaredMethods.find(_.name == "validate") match
      case None        => return inputExpr.asExprOf[T]
      case Some(value) => value

    val isValidateInline = validateMethod.flags.is(Flags.Inline)
    if !isValidateInline then
      report.errorAndAbort(
        ErrorMessages.validateIsNotInlineMessage(inputExpr, nt, validateMethod.pos)
      )

    lazy val treeSource =
      try validateMethod.tree.pos.sourceCode
      catch case _: Throwable => None

    inputExpr match
      case Eval(eval) =>
        scala.util.Try(eval.result(using Map.empty)) match
          case Failure(_) =>
            report.errorAndAbort(ErrorMessages.inputParseFailureMessage(inputExpr, nt))
          case Success(_) =>
            ()
      case _ =>
        report.errorAndAbort(ErrorMessages.inputParseFailureMessage(inputExpr, nt))

    val validateApplied = Expr.betaReduce('{ $validate($inputExpr) })
    validateApplied match
      case Eval(eval) =>
        scala.util.Try(eval.result(using Map.empty)) match
          case Failure(exception) =>
            val missingReference = exception match
              case EvalError.MissingReference(name) => Some(name)
              case _                                => None

            // throw exception
            // TODO: Add fatal exception error message
            // TODO: Have more specific errors for unknown method calls, functions, etc.
            report.errorAndAbort(ErrorMessages.failedToParseValidateMethod(inputExpr, nt, treeSource, missingReference))

          case Success(true) =>
            inputExpr.asExprOf[T]

          case Success(false) =>
            lazy val expressionSource: Option[String] =
              validate.asTerm match
                case Uninlined(Block(_, Lambda(_, Seal(Eval(eval))))) =>
                  Some(eval.render(using Map("INPUT" -> "input".blue)))
                case _ =>
                  None

            report.errorAndAbort(ErrorMessages.validationFailureMessage(inputExpr, nt, expressionSource, None))

          case Success(errorMessage: String) =>
            report.errorAndAbort(ErrorMessages.validationFailureMessage(inputExpr, nt, None, Some(errorMessage)))

      case other =>
        report.errorAndAbort(ErrorMessages.failedToParseValidateMethod(inputExpr, nt, treeSource, None))

  def applyAllImpl[A: Type, T: Type, NT <: Neotype[A] { type Type = T }: Type](
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
      case Eval[A](eval) =>
        val result      = eval.result(using Map.empty)
        given ToExpr[A] = toExprInstance(result).asInstanceOf[ToExpr[A]]
        Expr(result)
      case _ =>
        report.errorAndAbort(s"Could not parse input at compile time: ${expr.show}\n\n${expr.asTerm.toString.blue}")
        ???

  def toExprInstance(using Quotes)(any: Any): ToExpr[?] =
    import quotes.reflect.*
    any match
      case _: Int                  => summon[ToExpr[Int]]
      case _: String               => summon[ToExpr[String]]
      case _: Boolean              => summon[ToExpr[Boolean]]
      case _: Long                 => summon[ToExpr[Long]]
      case _: Double               => summon[ToExpr[Double]]
      case _: Float                => summon[ToExpr[Float]]
      case _: Char                 => summon[ToExpr[Char]]
      case _: Byte                 => summon[ToExpr[Byte]]
      case _: Short                => summon[ToExpr[Short]]
      case _: Set[Int @unchecked]  => summon[ToExpr[Set[Int]]]
      case _: List[Int @unchecked] => summon[ToExpr[List[Int]]]
      case _: BigInt               => summon[ToExpr[BigInt]]
      case _: BigDecimal           => summon[ToExpr[BigDecimal]]
