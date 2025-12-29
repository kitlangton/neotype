package comptime

import PatternNames.*
import PatternReflection.*

object PatternUnapplySimple:
  def unapplyPattern(
      fullName: String,
      recv: Option[TermIR],
      v: Any,
      args: List[PatternIR],
      evalTerm: TermIR => Either[ComptimeError, Any]
  )(
      matchesFn: (PatternIR, Any, TermIR => Either[ComptimeError, Any]) => Either[ComptimeError, Option[
        Map[String, Any]
      ]]
  ): Either[ComptimeError, Option[Map[String, Any]]] =
    val normalized = stripExtractor(fullName)
    def loopArgs(values: List[Any]): Either[ComptimeError, Option[Map[String, Any]]] =
      PatternMatch.matchAll(args, values, evalTerm)(matchesFn)

    def valuesFromUnapply(value: Any): List[Any] =
      if args.size == 1 then List(value)
      else
        value match
          case prod: Product =>
            (0 until prod.productArity).toList.map(prod.productElement)
          case _ =>
            List(value)

    def handleUnapplyResult(result: Any): Either[ComptimeError, Option[Map[String, Any]]] =
      result match
        case b: java.lang.Boolean =>
          Right(if b.booleanValue && args.isEmpty then Some(Map.empty) else None)
        case b: Boolean =>
          Right(if b && args.isEmpty then Some(Map.empty) else None)
        case opt: Option[?] =>
          opt match
            case Some(value) => loopArgs(valuesFromUnapply(value))
            case None        => Right(None)
        case other =>
          loopArgs(valuesFromUnapply(other))

    if args.exists(_.isInstanceOf[PatternIR.SeqWildcard]) then Right(None)
    else
      v match
        case cc: CaseClassValue if classNameMatches(normalized, cc.fullName) =>
          loopArgs(cc.fields.map(_._2).toList)
        case _ =>
          recv match
            case Some(recvTerm) =>
              invokeInstanceMethod(recvTerm, "unapply", v, evalTerm) match
                case Left(err)           => Left(err)
                case Right(Some(result)) => handleUnapplyResult(result)
                case Right(None)         => Right(None)
            case None =>
              PatternUnapplySpecs.matchSimple(normalized, args, v, loopArgs) match
                case Some(result) =>
                  result
                case None =>
                  invokeModuleMethod(normalized, "unapply", v) match
                    case Left(err) => Left(err)
                    case Right(Some(result)) =>
                      handleUnapplyResult(result)
                    case Right(None) =>
                      PatternUnapplyFallback.moduleOrClassFallback(normalized, fullName, v, args, loopArgs)
