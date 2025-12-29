package comptime

private[comptime] object PatternUnapplySeqResult:
  def handleResult(
      result: Any,
      args: List[PatternIR],
      evalTerm: TermIR => Either[ComptimeError, Any],
      matchesFn: (PatternIR, Any, TermIR => Either[ComptimeError, Any]) => Either[ComptimeError, Option[
        Map[String, Any]
      ]]
  ): Either[ComptimeError, Option[Map[String, Any]]] =
    def matchSeqValues(values: List[Any], fixedPrefix: Int): Either[ComptimeError, Option[Map[String, Any]]] =
      PatternUnapplySeqMatch.matchSeqValues(args, values, fixedPrefix, evalTerm, matchesFn)

    result match
      case opt: Option[?] =>
        opt match
          case Some(value) =>
            PatternUnapplySeqMatch.valuesFromUnapplySeq(value) match
              case Some(values) => matchSeqValues(values.values, values.fixedPrefix)
              case None         => Left(ComptimeError.UnsupportedPattern("unapplySeq", s"result: ${value.getClass.getName}"))
          case None =>
            Right(None)
      case otherValue =>
        PatternUnapplySeqMatch.valuesFromUnapplySeq(otherValue) match
          case Some(values) => matchSeqValues(values.values, values.fixedPrefix)
          case None         => Left(ComptimeError.UnsupportedPattern("unapplySeq", s"result: ${otherValue.getClass.getName}"))
