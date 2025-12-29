package comptime

import util.TypeNames

final case class RuleContext(
    foldConstants: Boolean,
    compileTerm: TermIR => Either[ComptimeError, Eval],
    compileTermLazy: TermIR => Either[ComptimeError, Eval]
)

sealed trait RuleSpec

final case class CallRule(
    id: String,
    recv: RecvPred,
    name: NamePred,
    arity: Arity,
    args: ArgShape,
    compile: CallCompiler
) extends RuleSpec

sealed trait RecvPred
case object AnyRecv                                extends RecvPred
final case class TypeRecv(fullName: String)        extends RecvPred
final case class UnionRecv(fullNames: Set[String]) extends RecvPred

sealed trait NamePred
case object AnyName                          extends NamePred
final case class NameIs(value: String)       extends NamePred
final case class NameIn(values: Set[String]) extends NamePred

sealed trait Arity
case object A0                            extends Arity
case object A1                            extends Arity
case object A2                            extends Arity
case object A3                            extends Arity
case object A4                            extends Arity
case object A5                            extends Arity
case object A1_1                          extends Arity
case object A1_1_1                        extends Arity
case object A1_2                          extends Arity
final case class ASet(values: Set[Arity]) extends Arity

sealed trait ArgShape
case object ByValue                extends ArgShape
final case class ByName(idx: Int)  extends ArgShape
final case class Varargs(idx: Int) extends ArgShape

// Compiler signatures
// NOTE: keep them in RuleSpec to avoid circular dependencies.
type CallCompiler = (CallIR, RuleContext) => Either[ComptimeError, Eval]

object RuleDsl:
  final case class RuleBuilder(
      names: Set[String],
      recv: RecvPred = AnyRecv,
      arity: Arity = ASet(Set.empty),
      args: ArgShape = ByValue
  ):
    def recv(pred: RecvPred): RuleBuilder = copy(recv = pred)
    def on(pred: RecvPred): RuleBuilder   = recv(pred)

    def arity(values: Arity*): RuleBuilder =
      val flattened = values.flatMap {
        case ASet(inner) => inner
        case other       => Set(other)
      }.toSet
      copy(arity = ASet(flattened))
    def anyArity: RuleBuilder              = arity()
    def a0: RuleBuilder                    = arity(A0)
    def a1: RuleBuilder                    = arity(A1)
    def a2: RuleBuilder                    = arity(A2)
    def a3: RuleBuilder                    = arity(A3)
    def a1_1: RuleBuilder                  = arity(A1_1)
    def a1_1_1: RuleBuilder                = arity(A1_1_1)
    def a1_2: RuleBuilder                  = arity(A1_2)
    def args(shape: ArgShape): RuleBuilder = copy(args = shape)
    def compile(id: String)(f: CallCompiler): CallRule =
      CallRule(id = id, recv = recv, name = NameIn(names), arity = arity, args = args, compile = f)
    def compile(f: CallCompiler): CallRule =
      compile(defaultId)(f)

    private def defaultId: String =
      val nameKey = names.toList.sorted.mkString("|")
      val recvKey = recv match
        case AnyRecv              => "*"
        case TypeRecv(fullName)   => fullName
        case UnionRecv(fullNames) => fullNames.toList.sorted.mkString("{", ",", "}")
      s"$recvKey.$nameKey"

  def rule(name: String, rest: String*): RuleBuilder =
    RuleBuilder((name :: rest.toList).toSet)

  def int: RecvPred    = TypeRecv(TypeNames.intName)
  def long: RecvPred   = TypeRecv(TypeNames.longName)
  def float: RecvPred  = TypeRecv(TypeNames.floatName)
  def double: RecvPred = TypeRecv(TypeNames.doubleName)
  def char: RecvPred   = TypeRecv(TypeNames.charName)
  def bool: RecvPred   = TypeRecv(TypeNames.boolName)
  def byte: RecvPred   = TypeRecv(TypeNames.byteName)
  def short: RecvPred  = TypeRecv(TypeNames.shortName)
  def string: RecvPred = UnionRecv(TypeNames.stringReceivers)
