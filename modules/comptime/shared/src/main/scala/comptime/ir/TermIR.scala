package comptime

sealed trait TermIR

object TermIR:
  final case class Lit(value: Any)                                   extends TermIR
  final case class Ref(name: String, fullName: Option[String])       extends TermIR
  final case class Val(name: String, value: TermIR)                  extends TermIR
  final case class Block(stats: List[TermIR], expr: TermIR)          extends TermIR
  final case class Lambda(params: List[ParamIR], body: TermIR)       extends TermIR
  final case class If(cond: TermIR, onTrue: TermIR, onFalse: TermIR) extends TermIR
  final case class Call(call: CallIR)                                extends TermIR
  final case class CaseClass(
      fullName: String,
      fields: List[String],
      repeatedIndex: Option[Int],
      args: List[TermIR]
  ) extends TermIR
  final case class Match(scrutinee: TermIR, cases: List[CaseIR])                     extends TermIR
  final case class Throw(expr: TermIR)                                               extends TermIR
  final case class Try(expr: TermIR, cases: List[CaseIR], finalizer: Option[TermIR]) extends TermIR
