package comptime

import PatternNames.*
import PatternReflection.*

object PatternUnapplySeq:
  def unapplySeqPattern(
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
    def matchSeqValues(values: List[Any], fixedPrefix: Int): Either[ComptimeError, Option[Map[String, Any]]] =
      PatternUnapplySeqMatch.matchSeqValues(args, values, fixedPrefix, evalTerm, matchesFn)

    v match
      case cc: CaseClassValue if classNameMatches(normalized, cc.fullName) =>
        PatternUnapplySeqMatch.valuesFromUnapplySeq(cc) match
          case Some(values) => matchSeqValues(values.values, values.fixedPrefix)
          case None         => Right(None)
      case _ =>
        recv match
          case Some(recvTerm) =>
            invokeInstanceMethod(recvTerm, "unapplySeq", v, evalTerm) match
              case Left(err) => Left(err)
              case Right(Some(result)) =>
                PatternUnapplySeqResult.handleResult(result, args, evalTerm, matchesFn)
              case Right(None) =>
                Right(None)
          case None =>
            PatternUnapplySpecs.matchSeq(normalized, v) match
              case Some(values) =>
                matchSeqValues(values.values, values.fixedPrefix)
              case None =>
                invokeModuleMethod(normalized, "unapplySeq", v) match
                  case Left(err) => Left(err)
                  case Right(Some(result)) =>
                    PatternUnapplySeqResult.handleResult(result, args, evalTerm, matchesFn)
                  case Right(None) =>
                    Left(ComptimeFailure.UnsupportedPattern("unapplySeq", normalized))
