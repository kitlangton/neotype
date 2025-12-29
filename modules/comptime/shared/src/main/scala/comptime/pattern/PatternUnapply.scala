package comptime

object PatternUnapply:
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
    PatternUnapplySimple.unapplyPattern(fullName, recv, v, args, evalTerm)(matchesFn)

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
    PatternUnapplySeq.unapplySeqPattern(fullName, recv, v, args, evalTerm)(matchesFn)
