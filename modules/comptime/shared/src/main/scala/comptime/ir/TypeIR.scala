package comptime

sealed trait TypeIR

object TypeIR:
  final case class Ref(fullName: String, args: List[TypeIR]) extends TypeIR
  final case class AnyType()                                 extends TypeIR
