package comptime

private[comptime] sealed trait PatternIR

private[comptime] object PatternIR:
  final case class Wildcard()                         extends PatternIR
  final case class Literal(value: Any)                extends PatternIR
  final case class Bind(name: String, pat: PatternIR) extends PatternIR
  final case class SeqWildcard(bind: Option[String])  extends PatternIR
  final case class Alt(patterns: List[PatternIR])     extends PatternIR
  final case class Tuple(elems: List[PatternIR])      extends PatternIR
  final case class Unapply(fullName: String, recv: Option[TermIR], args: List[PatternIR], isSeq: Boolean)
      extends PatternIR
  final case class Typed(tpe: TypeIR, pat: PatternIR) extends PatternIR
