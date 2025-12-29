package comptime

private[comptime] sealed trait TypeIR

private[comptime] object TypeIR:
  final case class Ref(fullName: String, args: List[TypeIR]) extends TypeIR
  final case class AnyType()                                 extends TypeIR
