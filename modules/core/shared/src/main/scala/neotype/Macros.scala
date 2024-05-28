package neotype

import neotype.eval.Eval
import neotype.eval.EvalError
import neotype.eval.Seal
import neotype.eval.Uninlined
import neotype.eval.Unseal

import scala.quoted.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import StringFormatting.*

private[neotype] object Macros:

  def applyImpl[Input: Type, T: Type, NT <: TypeWrapper[Input] { type Type = T }: Type](
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
        // try
        val result      = eval.result(using Map.empty)
        given ToExpr[A] = toExprType[A]
        Expr(result)
//         catch
//           case e: Throwable =>
//             report.errorAndAbort(s"""
// --------------
// error: ${e}
// Failed to evaluate expression at compile time
// SHOW: ${expr.show}

// TERM: ${expr.asTerm}

// EVAL: ${eval}
// --------------
//               """)
      case _ =>
        report.errorAndAbort(s"Could not parse input at compile time: ${expr.show}\n\n${expr.asTerm.toString.blue}")
        ???

  def toExprType[A: Type](using Quotes): ToExpr[A] =
    import quotes.reflect.*
    Type.of[A] match
      case '[Int]        => summon[ToExpr[Int]].asInstanceOf[ToExpr[A]]
      case '[String]     => summon[ToExpr[String]].asInstanceOf[ToExpr[A]]
      case '[Boolean]    => summon[ToExpr[Boolean]].asInstanceOf[ToExpr[A]]
      case '[Long]       => summon[ToExpr[Long]].asInstanceOf[ToExpr[A]]
      case '[Double]     => summon[ToExpr[Double]].asInstanceOf[ToExpr[A]]
      case '[Float]      => summon[ToExpr[Float]].asInstanceOf[ToExpr[A]]
      case '[Char]       => summon[ToExpr[Char]].asInstanceOf[ToExpr[A]]
      case '[Byte]       => summon[ToExpr[Byte]].asInstanceOf[ToExpr[A]]
      case '[Short]      => summon[ToExpr[Short]].asInstanceOf[ToExpr[A]]
      case '[BigInt]     => summon[ToExpr[BigInt]].asInstanceOf[ToExpr[A]]
      case '[BigDecimal] => summon[ToExpr[BigDecimal]].asInstanceOf[ToExpr[A]]
      case '[Unit]       => summon[ToExpr[Unit]].asInstanceOf[ToExpr[A]]
      case '[Set[a]] =>
        given ToExpr[a] = toExprType[a]
        summon[ToExpr[Set[a]]].asInstanceOf[ToExpr[A]]
      case '[List[a]] =>
        given ToExpr[a] = toExprType[a]
        summon[ToExpr[List[a]]].asInstanceOf[ToExpr[A]]
      case '[Vector[a]] =>
        given ToExpr[a] = toExprType[a]
        summon[ToExpr[Vector[a]]].asInstanceOf[ToExpr[A]]
      case '[Option[a]] =>
        given ToExpr[a] = toExprType[a]
        summon[ToExpr[Option[a]]].asInstanceOf[ToExpr[A]]
      case '[Either[e, a]] =>
        given ToExpr[e] = toExprType[e]
        given ToExpr[a] = toExprType[a]
        summon[ToExpr[Either[e, a]]].asInstanceOf[ToExpr[A]]
      case '[Map[k, v]] =>
        given ToExpr[k] = toExprType[k]
        given ToExpr[v] = toExprType[v]
        summon[ToExpr[Map[k, v]]].asInstanceOf[ToExpr[A]]
      case '[(a, b)] =>
        given ToExpr[a] = toExprType[a]
        given ToExpr[b] = toExprType[b]
        summon[ToExpr[(a, b)]].asInstanceOf[ToExpr[A]]
      case '[Iterator[a]] =>
        given ToExpr[a] = toExprType[a]
        summon[ToExpr[Iterator[a]]].asInstanceOf[ToExpr[A]]
      case '[Iterable[a]] =>
        given ToExpr[a] = toExprType[a]
        summon[ToExpr[Iterable[a]]].asInstanceOf[ToExpr[A]]
      case '[scala.util.Try[a]] =>
        given ToExpr[a] = toExprType[a]
        summon[ToExpr[scala.util.Try[a]]].asInstanceOf[ToExpr[A]]
      case _ =>
        val typeRepr = TypeRepr.of[A]
        typeRepr match
          // A | B
          case OrType(left, right) =>
            (left.asType, right.asType) match
              case ('[a], '[b]) =>
                given ToExpr[a] = toExprType[a]
                given ToExpr[b] = toExprType[b]
                OrTypeToExpr[a, b].asInstanceOf[ToExpr[A]]
          case _ =>
            report.errorAndAbort(s"toExprType failed for: ${typeRepr.show}")

  given VectorToExpr[T: Type: ToExpr]: ToExpr[Vector[T]] with
    def apply(xs: Vector[T])(using Quotes): Expr[Vector[T]] =
      '{ Vector(${ Varargs(xs.map(summon[ToExpr[T]].apply)) }*) }

  given MapToExpr[K: Type: ToExpr, V: Type: ToExpr]: ToExpr[Map[K, V]] with
    def apply(m: Map[K, V])(using Quotes): Expr[Map[K, V]] =
      '{
        Map(${
          Varargs(m.map { case (k, v) =>
            '{ ${ summon[ToExpr[K]].apply(k) } -> ${ summon[ToExpr[V]].apply(v) } }
          }.toList)
        }*)
      }

  given IterableToExpr[T: Type: ToExpr]: ToExpr[Iterable[T]] with
    def apply(xs: Iterable[T])(using Quotes): Expr[Iterable[T]] =
      '{ Iterable(${ Varargs(xs.map(summon[ToExpr[T]].apply).toSeq) }*) }

  given IteratorToExpr[T: Type: ToExpr]: ToExpr[Iterator[T]] with
    def apply(xs: Iterator[T])(using Quotes): Expr[Iterator[T]] =
      '{ Iterator(${ Varargs(xs.map(summon[ToExpr[T]].apply).toSeq) }*) }

  given TryToExpr[A: Type: ToExpr]: ToExpr[Try[A]] with
    def apply(t: Try[A])(using Quotes): Expr[Try[A]] =
      t match
        case Success(a) => '{ scala.util.Success(${ summon[ToExpr[A]].apply(a) }) }
        case Failure(e) => '{ scala.util.Failure(${ summon[ToExpr[Throwable]].apply(e) }) }

  given ThrowableToExpr: ToExpr[Throwable] with
    def apply(e: Throwable)(using Quotes): Expr[Throwable] =
      '{ new Throwable(${ Expr(e.getMessage()) }) }

  def OrTypeToExpr[A: Type: ToExpr, B: Type: ToExpr]: ToExpr[A | B] = new:
    def apply(aOrB: A | B)(using Quotes): Expr[A | B] =
      try Expr(aOrB.asInstanceOf[A])
      catch case _ => Expr(aOrB.asInstanceOf[B])

  given UnitToExpr: ToExpr[Unit] with
    def apply(x: Unit)(using Quotes): Expr[Unit] = '{ () }
