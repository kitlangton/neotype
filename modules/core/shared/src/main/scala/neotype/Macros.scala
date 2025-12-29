package neotype

import comptime.Compiler as ComptimeCompiler
import comptime.ComptimeError
import comptime.ComptimeFailure
import comptime.Eval as ComptimeEval
import comptime.MacroExprs
import comptime.ScalaAstBridge

import scala.quoted.*
import scala.util.Failure
import scala.util.Success

private[neotype] object Macros:
  private object Uninlined:
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect.*
      term match
        case Inlined(_, bindings, t) => Some(Block(bindings, t))
        case t                       => Some(t)

  private def peel(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case Inlined(_, _, t) =>
        peel(t)
      case Typed(t, _) =>
        peel(t)
      case Block(Nil, t) =>
        peel(t)
      case other =>
        other

  def applyImpl[Input: Type, T: Type, NT <: TypeWrapper[Input] { type Type = T }: Type](
      inputExpr: Expr[Input],
      validate: Expr[Input => Boolean | String]
  )(using Quotes): Expr[T] =
    import quotes.reflect.*

    def isTypeWrapper(tpe: TypeRepr): Boolean =
      tpe.baseClasses.exists { sym =>
        sym.fullName == "neotype.TypeWrapper" ||
        sym.fullName == "neotype.Newtype" ||
        sym.fullName == "neotype.Subtype"
      } ||
        tpe.typeSymbol.methodMembers.exists(_.name == "validate")

    def stripNeotypeWrappers(term: Term): Term =
      peel(term) match
        case Apply(fun, List(arg)) =>
          fun match
            case Select(recv, "apply") if isTypeWrapper(recv.tpe) =>
              stripNeotypeWrappers(arg)
            case id: Ident if isTypeWrapper(id.tpe) =>
              stripNeotypeWrappers(arg)
            case _ =>
              term
        case _ =>
          term

    def compileTerm(term: quotes.reflect.Term): Either[ComptimeError, ComptimeEval] =
      try ComptimeCompiler.compileTerm(ScalaAstBridge.termToIR(term))
      catch case e: Throwable => Left(ComptimeFailure.EvalException(e.getClass.getSimpleName, e.getMessage))

    lazy val nt = TypeRepr.of[NT].widenTermRefByName match
      case Refinement(t, _, _) => t
      case t                   => t

    // Find validate method that overrides TypeWrapper.validate in the hierarchy.
    // This supports the pattern where validate is defined in a parent trait.
    // Uses allOverriddenSymbols to ensure we find the actual override, not an unrelated method.
    def findValidateOverride(sym: Symbol): Option[Symbol] =
      def isTypeWrapperValidate(s: Symbol): Boolean =
        s.owner.fullName == "neotype.TypeWrapper" && s.name == "validate"

      def overridesTypeWrapperValidate(method: Symbol): Boolean =
        method.name == "validate" && method.allOverriddenSymbols.exists(isTypeWrapperValidate)

      // Check this type and all its base classes for a validate override
      (sym :: sym.typeRef.baseClasses).iterator
        .flatMap(_.declaredMethods.find(overridesTypeWrapperValidate))
        .nextOption()

    val validateMethod = findValidateOverride(nt.typeSymbol) match
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

    val inputCompile =
      val normalized = stripNeotypeWrappers(inputExpr.asTerm)
      compileTerm(normalized) match
        case right @ Right(_) => right
        case Left(_) =>
          val normalizedArg = stripNeotypeWrappers(inputExpr.asTerm.underlyingArgument)
          compileTerm(normalizedArg)

    inputCompile match
      case Right(eval) =>
        scala.util.Try(ComptimeEval.run(eval)) match
          case Failure(_) =>
            report.errorAndAbort(ErrorMessages.inputParseFailureMessage(inputExpr, nt))
          case Success(_) =>
            ()
      case Left(_) =>
        report.errorAndAbort(ErrorMessages.inputParseFailureMessage(inputExpr, nt))

    val validateApplied = Expr.betaReduce('{ $validate($inputExpr) })

    val validateEvalE = compileTerm(validateApplied.asTerm)

    validateEvalE match
      case Right(eval) =>
        scala.util.Try(ComptimeEval.run(eval)) match
          case Failure(exception) =>
            report.errorAndAbort(
              ErrorMessages.failedToParseValidateMethod(inputExpr, nt, treeSource, None, Some(exception.toString))
            )

          case Success(true) =>
            inputExpr.asExprOf[T]

          case Success(false) =>
            lazy val expressionSource: Option[String] =
              validate.asTerm match
                case Uninlined(Block(_, Lambda(_, body))) =>
                  body.pos.sourceCode
                case _ =>
                  None

            report.errorAndAbort(ErrorMessages.validationFailureMessage(inputExpr, nt, expressionSource, None))

          case Success(errorMessage: String) =>
            report.errorAndAbort(ErrorMessages.validationFailureMessage(inputExpr, nt, None, Some(errorMessage)))

          case Success(_) =>
            report.errorAndAbort(
              ErrorMessages.failedToParseValidateMethod(inputExpr, nt, treeSource, None, None)
            )

      case Left(err) =>
        report.errorAndAbort(
          ErrorMessages.failedToParseValidateMethod(inputExpr, nt, treeSource, None, Some(ComptimeError.format(err)))
        )

  def applyAllImpl[A: Type, T: Type, NT <: TypeWrapper[A] { type Type = T }: Type](
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
      case _ =>
        peel(as.asTerm).asExprOf[Seq[A]] match
          case Varargs(args) =>
            processArgs(args)
          case other =>
            report.errorAndAbort(s"Could not parse input at compile time: ${other.show}")

  def parserNameImpl[P: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    def unwrap(tpe: TypeRepr): TypeRepr =
      tpe.widenTermRefByName match
        case Refinement(t, _, _) => unwrap(t)
        case t                   => t
    val name = unwrap(TypeRepr.of[P]).typeSymbol.name.replaceAll("\\$$", "")
    Expr(name)

  def parseApplyImpl[In: Type, Out: Type, P <: ParsedFrom[In, Out]: Type](
      inputExpr: Expr[In],
      parse: Expr[In => Either[String, Out]]
  )(using Quotes): Expr[Out] =
    import quotes.reflect.*

    def peelTerm(term: Term): Term =
      peel(term)

    def compileTerm(term: quotes.reflect.Term): Either[ComptimeError, ComptimeEval] =
      try ComptimeCompiler.compileTerm(ScalaAstBridge.termToIR(term))
      catch case e: Throwable => Left(ComptimeFailure.EvalException(e.getClass.getSimpleName, e.getMessage))

    def unwrap(tpe: TypeRepr): TypeRepr =
      tpe.widenTermRefByName match
        case Refinement(t, _, _) => unwrap(t)
        case t                   => t

    val parserTpe  = unwrap(TypeRepr.of[P])
    val parserName = parserTpe.typeSymbol.name.replaceAll("\\$$", "")

    def findParseOverride(sym: Symbol): Option[Symbol] =
      def isParsedFromParse(s: Symbol): Boolean =
        s.owner.fullName == "neotype.ParsedFrom" && s.name == "parse"

      def overridesParsedFromParse(method: Symbol): Boolean =
        method.name == "parse" && method.allOverriddenSymbols.exists(isParsedFromParse)

      (sym :: sym.typeRef.baseClasses).iterator
        .flatMap(_.declaredMethods.find(overridesParsedFromParse))
        .nextOption()

    val parseMethod = findParseOverride(parserTpe.typeSymbol).getOrElse {
      report.errorAndAbort(s"$parserName has no parse method")
    }

    if !parseMethod.flags.is(Flags.Inline) then
      report.errorAndAbort(
        ErrorMessages.parseNotInlineMessage(inputExpr, parserName, parseMethod.pos)
      )

    val inputCompile =
      val normalized = peelTerm(inputExpr.asTerm)
      compileTerm(normalized) match
        case right @ Right(_) => right
        case Left(_) =>
          val normalizedArg = peelTerm(inputExpr.asTerm.underlyingArgument)
          compileTerm(normalizedArg)

    inputCompile match
      case Right(eval) =>
        scala.util.Try(ComptimeEval.run(eval)) match
          case Failure(_) =>
            report.errorAndAbort(ErrorMessages.parseInputNotConstantMessage(inputExpr, parserName))
          case Success(_) =>
            ()
      case Left(_) =>
        report.errorAndAbort(ErrorMessages.parseInputNotConstantMessage(inputExpr, parserName))

    val parseApplied = Expr.betaReduce('{ $parse($inputExpr) })
    val parseEvalE   = compileTerm(parseApplied.asTerm)

    parseEvalE match
      case Right(eval) =>
        scala.util.Try(ComptimeEval.run(eval)) match
          case Failure(exception) =>
            report.errorAndAbort(
              ErrorMessages.parseMethodFailedMessage(parserName, Some(exception.toString))
            )

          case Success(Right(value)) =>
            MacroExprs.summonExprOpt[Out] match
              case Some(te) =>
                te.asInstanceOf[ToExpr[Any]].apply(value).asExprOf[Out]
              case None =>
                report.errorAndAbort(
                  ComptimeFailure.format(ComptimeFailure.CannotLift(Type.show[Out]))
                )

          case Success(Left(message: String)) =>
            report.errorAndAbort(
              ErrorMessages.parseFailureMessage(inputExpr, parserName, message)
            )

          case Success(other) =>
            report.errorAndAbort(
              ErrorMessages.parseMethodFailedMessage(
                parserName,
                Some(s"Expected Either[String, ${Type.show[Out]}], got: ${other}")
              )
            )

      case Left(err) =>
        report.errorAndAbort(
          ErrorMessages.parseMethodFailedMessage(parserName, Some(ComptimeError.format(err)))
        )
