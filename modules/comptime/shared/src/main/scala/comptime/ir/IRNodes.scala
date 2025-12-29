package comptime

private[comptime] final case class CaseIR(pattern: PatternIR, guard: Option[TermIR], body: TermIR)

private[comptime] final case class ParamIR(name: String, tpe: TypeIR)

private[comptime] final case class CallIR(
    recv: TermIR,
    owner: String,
    name: String,
    targs: List[TypeIR],
    args: List[List[TermIR]],
    pos: Option[SourcePos] = None
)
