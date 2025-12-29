package comptime

import scala.reflect.ClassTag

import RuleHelpers.*

// === Small helpers for building rule lists ===
private[comptime] object RuleOpsSupport:
  def mapPairs[T](pairs: Seq[(String, T)])(f: (String, T) => CallRule): List[CallRule] =
    pairs.toList.map { case (name, value) => f(name, value) }

  def mapTriples[A1, A2](pairs: Seq[(String, A1, A2)])(f: (String, A1, A2) => CallRule): List[CallRule] =
    pairs.toList.map { case (name, a1, a2) => f(name, a1, a2) }

  def mapNames(names: Seq[String])(f: String => CallRule): List[CallRule] =
    names.toList.map(f)

// === Ops helpers (by-value) ===
private[comptime] trait RulesForOps[A]:
  self: RulesFor[A] =>

  import RuleOpsSupport.{mapNames, mapPairs}

  private type Op0[R]          = A => R
  private type Op1[B, R]       = (A, B) => R
  private type Op2[B, C, R]    = (A, B, C) => R
  private type Op3[B, C, D, R] = (A, B, C, D) => R

  private def ops1Pairs[B: ClassTag](pairs: Seq[(String, Op1[B, Any])]): List[CallRule] =
    mapPairs(pairs)((name, f) => rule1[B, Any](name)(f))

  private def ops2Pairs[B: ClassTag, C: ClassTag](pairs: Seq[(String, Op2[B, C, Any])]): List[CallRule] =
    mapPairs(pairs)((name, f) => rule2[B, C, Any](name)(f))

  private def ops3Pairs[B: ClassTag, C: ClassTag, D: ClassTag](
      pairs: Seq[(String, Op3[B, C, D, Any])]
  ): List[CallRule] =
    mapPairs(pairs)((name, f) => rule3[B, C, D, Any](name)(f))

  def ops(pairs: (String, Op0[Any])*): List[CallRule] =
    mapPairs(pairs)((name, f) => rule0[Any](name)(f))

  def opsList(pairs: Seq[(String, Op0[Any])]): List[CallRule] =
    ops(pairs*)

  // Accept Any => Any functions directly (for cases where receiver value may be widened at runtime)
  def opsAnyRecv(pairs: (String, Any => Any)*): List[CallRule] =
    mapPairs(pairs)((name, f) => RuleHelpersCore.ruleRecv0Untyped(self.recv, name)(f))

  def opsAnyRecvList(pairs: Seq[(String, Any => Any)]): List[CallRule] =
    opsAnyRecv(pairs*)

  def opsByName(names: String*)(f: String => Op0[Any]): List[CallRule] =
    mapNames(names)(name => rule0[Any](name)(f(name)))

  def opsId(names: String*): List[CallRule] =
    mapNames(names.toList)(name => rule0[Any](name)(identity))

  def ops1[B: ClassTag](pairs: (String, Op1[B, Any])*): List[CallRule] =
    ops1Pairs(pairs)

  def ops1List[B: ClassTag](pairs: Seq[(String, Op1[B, Any])]): List[CallRule] =
    ops1(pairs*)

  def ops1Any(pairs: (String, (A, Any) => Any)*): List[CallRule] =
    ops1[Any](pairs*)

  def ops1AnyList(pairs: Seq[(String, (A, Any) => Any)]): List[CallRule] =
    ops1Any(pairs*)

  def ops1Int(pairs: (String, (A, Int) => Any)*): List[CallRule] =
    ops1[Int](pairs*)

  def ops1IntList(pairs: Seq[(String, (A, Int) => Any)]): List[CallRule] =
    ops1Int(pairs*)

  def ops1Fn(pairs: (String, Op1[Any => Any, Any])*): List[CallRule] =
    ops1[Any => Any](pairs*)

  def ops1FnList(pairs: Seq[(String, Op1[Any => Any, Any])]): List[CallRule] =
    ops1Fn(pairs*)

  def ops1FnOf[F](pairs: (String, Op1[Any => F, Any])*): List[CallRule] =
    ops1[Any => F](pairs*)

  def ops1FnOfList[F](pairs: Seq[(String, Op1[Any => F, Any])]): List[CallRule] =
    ops1FnOf[F](pairs*)

  def ops1Pred(pairs: (String, Op1[Any => Boolean, Any])*): List[CallRule] =
    ops1[Any => Boolean](pairs*)

  def ops1PredList(pairs: Seq[(String, Op1[Any => Boolean, Any])]): List[CallRule] =
    ops1Pred(pairs*)

  def ops1CharPred(pairs: (String, Op1[Char => Boolean, Any])*): List[CallRule] =
    ops1[Char => Boolean](pairs*)

  def ops1CharPredList(pairs: Seq[(String, Op1[Char => Boolean, Any])]): List[CallRule] =
    ops1CharPred(pairs*)

  def ops1ThrowableFn(pairs: (String, Op1[Throwable => Any, Any])*): List[CallRule] =
    ops1[Throwable => Any](pairs*)

  def ops1ThrowableFnList(pairs: Seq[(String, Op1[Throwable => Any, Any])]): List[CallRule] =
    ops1ThrowableFn(pairs*)

  def ops1ByName[B: ClassTag](names: String*)(f: String => Op1[B, Any]): List[CallRule] =
    mapNames(names)(name => rule1[B, Any](name)(f(name)))

  def ops1ByNameAny(names: String*)(f: String => ((A, Any) => Any)): List[CallRule] =
    ops1ByName[Any](names*)(f)

  def ops2[B: ClassTag, C: ClassTag](pairs: (String, Op2[B, C, Any])*): List[CallRule] =
    ops2Pairs(pairs)

  def ops2List[B: ClassTag, C: ClassTag](pairs: Seq[(String, Op2[B, C, Any])]): List[CallRule] =
    ops2(pairs*)

  def ops2Any(pairs: (String, (A, Any, Any) => Any)*): List[CallRule] =
    ops2[Any, Any](pairs*)

  def ops2AnyList(pairs: Seq[(String, (A, Any, Any) => Any)]): List[CallRule] =
    ops2Any(pairs*)

  def ops2AnyInt(pairs: (String, (A, Any, Int) => Any)*): List[CallRule] =
    ops2[Any, Int](pairs*)

  def ops2AnyIntList(pairs: Seq[(String, (A, Any, Int) => Any)]): List[CallRule] =
    ops2AnyInt(pairs*)

  def ops2Fn(pairs: (String, Op2[Any => Any, Any => Any, Any])*): List[CallRule] =
    ops2[Any => Any, Any => Any](pairs*)

  def ops2FnList(pairs: Seq[(String, Op2[Any => Any, Any => Any, Any])]): List[CallRule] =
    ops2Fn(pairs*)

  def ops2ThrowableFn(pairs: (String, Op2[Throwable => Any, Any => Any, Any])*): List[CallRule] =
    ops2[Throwable => Any, Any => Any](pairs*)

  def ops2ThrowableFnList(pairs: Seq[(String, Op2[Throwable => Any, Any => Any, Any])]): List[CallRule] =
    ops2ThrowableFn(pairs*)

  def ops2IntInt(pairs: (String, (A, Int, Int) => Any)*): List[CallRule] =
    ops2[Int, Int](pairs*)

  def ops2IntIntList(pairs: Seq[(String, (A, Int, Int) => Any)]): List[CallRule] =
    ops2IntInt(pairs*)

  def ops2IntAny(pairs: (String, (A, Int, Any) => Any)*): List[CallRule] =
    ops2[Int, Any](pairs*)

  def ops2IntAnyList(pairs: Seq[(String, (A, Int, Any) => Any)]): List[CallRule] =
    ops2IntAny(pairs*)

  def ops2ByName[B: ClassTag, C: ClassTag](names: String*)(f: String => Op2[B, C, Any]): List[CallRule] =
    mapNames(names)(name => rule2[B, C, Any](name)(f(name)))

  def ops2ByNameAny(names: String*)(f: String => ((A, Any, Any) => Any)): List[CallRule] =
    ops2ByName[Any, Any](names*)(f)

  def ops3[B: ClassTag, C: ClassTag, D: ClassTag](pairs: (String, Op3[B, C, D, Any])*): List[CallRule] =
    ops3Pairs(pairs)

  def ops3List[B: ClassTag, C: ClassTag, D: ClassTag](pairs: Seq[(String, Op3[B, C, D, Any])]): List[CallRule] =
    ops3(pairs*)

  def ops3Any(pairs: (String, (A, Any, Any, Any) => Any)*): List[CallRule] =
    ops3[Any, Any, Any](pairs*)

  def ops3AnyList(pairs: Seq[(String, (A, Any, Any, Any) => Any)]): List[CallRule] =
    ops3Any(pairs*)

  def ops3ByName[B: ClassTag, C: ClassTag, D: ClassTag](names: String*)(
      f: String => Op3[B, C, D, Any]
  ): List[CallRule] =
    mapNames(names)(name => rule3[B, C, D, Any](name)(f(name)))

  def ops3ByNameAny(names: String*)(f: String => ((A, Any, Any, Any) => Any)): List[CallRule] =
    ops3ByName[Any, Any, Any](names*)(f)

  def rules[R](name: String, rest: String*)(f: A => R): List[CallRule] =
    mapNames(name :: rest.toList)(n => rule0(n)(f))

  def rules[B: ClassTag, R](name: String, rest: String*)(f: Op1[B, R]): List[CallRule] =
    mapNames(name :: rest.toList)(n => rule1[B, R](n)(f))

  def rules[B: ClassTag, C: ClassTag, R](name: String, rest: String*)(f: Op2[B, C, R]): List[CallRule] =
    mapNames(name :: rest.toList)(n => rule2[B, C, R](n)(f))

  def rules[B: ClassTag, C: ClassTag, D: ClassTag, R](
      name: String,
      rest: String*
  )(f: Op3[B, C, D, R]): List[CallRule] =
    mapNames(name :: rest.toList)(n => rule3[B, C, D, R](n)(f))

  def arg1[B: ClassTag, R](name: String)(f: B => R): CallRule =
    rule1[B, R](name)((_, b) => f(b))

  def arg1Any[R](name: String)(f: Any => R): CallRule =
    arg1[Any, R](name)(f)

  def arg1sId[B: ClassTag](names: String*): List[CallRule] =
    mapNames(names.toList)(name => arg1[B, B](name)(identity))

  def arg1s[B: ClassTag](pairs: (String, B => Any)*): List[CallRule] =
    mapPairs(pairs)((name, f) => arg1[B, Any](name)(f))

  def arg1sAny(pairs: (String, Any => Any)*): List[CallRule] =
    arg1s[Any](pairs*)

  def arg1sAnyList(pairs: Seq[(String, Any => Any)]): List[CallRule] =
    arg1sAny(pairs*)

  def arg2[B: ClassTag, C: ClassTag, R](name: String)(f: (B, C) => R): CallRule =
    rule2[B, C, R](name)((_, b, c) => f(b, c))

  def foldLeftAny(name: String)(f: (A, Any, (Any, Any) => Any) => Any): CallRule =
    rule1_1Or2[Any, (Any, Any) => Any, Any](name)(f)

  def foldRightAny(name: String)(f: (A, Any, (Any, Any) => Any) => Any): CallRule =
    rule1_1Or2[Any, (Any, Any) => Any, Any](name)(f)

  def reduceAny(name: String)(f: (A, (Any, Any) => Any) => Any): CallRule =
    rule1[(Any, Any) => Any, Any](name)(f)

  def sortWithAny(name: String)(f: (A, (Any, Any) => Boolean) => Any): CallRule =
    rule1[(Any, Any) => Boolean, Any](name)(f)

  def collectOps(f: (A, PartialFunction[Any, Any]) => Any): List[CallRule] =
    rulePf1s(
      "collect" -> f
    )

  def anyArityOps(pairs: (String, A => Any)*): List[CallRule] =
    rule0AnyAritys(pairs*)

  def anyArityOpsList(pairs: Seq[(String, A => Any)]): List[CallRule] =
    anyArityOps(pairs*)

  def anyArityOps1Fn(pairs: (String, (A, Any => Any) => Any)*): List[CallRule] =
    mapPairs(pairs)((name, f) => rule1AnyArity[Any => Any, Any](name)(f))

  def anyArityOps1FnList(pairs: Seq[(String, (A, Any => Any) => Any)]): List[CallRule] =
    anyArityOps1Fn(pairs*)

// === Ops helpers (by-name) ===
private[comptime] trait RulesForByName[A](using ClassTag[A]):
  self: RulesFor[A] =>

  import RuleOpsSupport.{mapPairs, mapTriples}

  private type Op1[B, R]        = (A, () => B) => R
  private type Op2[B, C, R]     = (A, B, () => C) => R
  private type Op11[B, C, R]    = (A, () => B, C) => R
  private type Op23[B, C, D, R] = (A, B, () => C, () => D) => R

  private def asBool(value: Any): Boolean =
    value.asInstanceOf[Boolean]

  private def byName_LPairs[B: ClassTag](pairs: Seq[(String, Op1[B, Any])]): List[CallRule] =
    mapPairs(pairs)((name, f) => byName_L[B, Any](name)(f))

  private def byName_SLPairs[B: ClassTag, C: ClassTag](
      pairs: Seq[(String, (A, B) => Option[Any], Op2[B, C, Any])]
  ): List[CallRule] =
    mapTriples(pairs) { (name, fast, f) =>
      byName_SL[B, C, Any](name)(fast)(f)
    }

  private def byName1_1_LSPairs[B: ClassTag, C: ClassTag](
      pairs: Seq[(String, (A, C) => Option[Any], Op11[B, C, Any])]
  ): List[CallRule] =
    mapTriples(pairs) { (name, fast, f) =>
      byName1_1_LS[B, C, Any](name)(fast)(f)
    }

  private def byName1_1_SLPairs[B: ClassTag, C: ClassTag](
      pairs: Seq[(String, (A, B) => Option[Any], Op2[B, C, Any])]
  ): List[CallRule] =
    mapTriples(pairs) { (name, fast, f) =>
      byName1_1_SL[B, C, Any](name)(fast)(f)
    }

  private def byName_SLLPairs[B: ClassTag, C: ClassTag, D: ClassTag](
      pairs: Seq[(String, (A, B) => Option[Any], Op23[B, C, D, Any])]
  ): List[CallRule] =
    mapTriples(pairs) { (name, fast, f) =>
      byName_SLL[B, C, D, Any](name)(fast)(f)
    }

  private def byNameCond_SL_Pairs(pairs: Seq[(String, (A, Boolean, () => Any) => Any)]): List[CallRule] =
    mapPairs(pairs) { (name, f) =>
      byName_SL[Any, Any, Any](name) { (a, b, c) =>
        f(a, asBool(b), c)
      }
    }

  private def byNameCond_SLL_Pairs(pairs: Seq[(String, (A, Boolean, () => Any, () => Any) => Any)]): List[CallRule] =
    mapPairs(pairs) { (name, f) =>
      byName_SLL[Any, Any, Any, Any](name) { (a, b, c, d) =>
        f(a, asBool(b), c, d)
      }
    }

  def byName_L[B: ClassTag, R](name: String)(f: Op1[B, R]): CallRule =
    ruleByName1Recv[A, B, R](recv, name)(f)

  def byName_LAny[R](name: String)(f: (A, () => Any) => R): CallRule =
    byName_L[Any, R](name)(f)

  def byName_Ls[B: ClassTag](pairs: (String, Op1[B, Any])*): List[CallRule] =
    byName_LPairs(pairs)

  def byName_LsList[B: ClassTag](pairs: Seq[(String, Op1[B, Any])]): List[CallRule] =
    byName_Ls(pairs*)

  def byName_LsAny(pairs: (String, (A, () => Any) => Any)*): List[CallRule] =
    byName_Ls[Any](pairs*)

  def byName_LsAnyList(pairs: Seq[(String, (A, () => Any) => Any)]): List[CallRule] =
    byName_LsAny(pairs*)

  def byName_SL[B: ClassTag, C: ClassTag, R](name: String)(fast: (A, B) => Option[R])(
      f: Op2[B, C, R]
  ): CallRule =
    ruleRecvByName_SL[A, B, C, R](recv, name)(fast)(f)

  def byName_SL[B: ClassTag, C: ClassTag, R](name: String)(f: Op2[B, C, R]): CallRule =
    ruleRecvByName_SL[A, B, C, R](recv, name)(f)

  def byName_SLs[B: ClassTag, C: ClassTag](
      pairs: (String, (A, B) => Option[Any], Op2[B, C, Any])*
  ): List[CallRule] =
    byName_SLPairs(pairs)

  def byName_SLsList[B: ClassTag, C: ClassTag](
      pairs: Seq[(String, (A, B) => Option[Any], Op2[B, C, Any])]
  ): List[CallRule] =
    byName_SLs(pairs*)

  def byName_SLsAny(
      pairs: (String, (A, Any) => Option[Any], (A, Any, () => Any) => Any)*
  ): List[CallRule] =
    byName_SLs[Any, Any](pairs*)

  def byName_SLsAnyList(
      pairs: Seq[(String, (A, Any) => Option[Any], (A, Any, () => Any) => Any)]
  ): List[CallRule] =
    byName_SLsAny(pairs*)

  // Like byName_SL but with by-name as the FIRST arg (not last)
  // Used for Option.fold(ifEmpty)(f) where ifEmpty is by-name
  def byName_LS[B: ClassTag, C: ClassTag, R](name: String)(fast: (A, C) => Option[R])(
      f: Op11[B, C, R]
  ): CallRule =
    ruleRecvByName_LS[A, B, C, R](recv, name)(fast)(f)

  def byName_LS[B: ClassTag, C: ClassTag, R](name: String)(f: Op11[B, C, R]): CallRule =
    ruleRecvByName_LS[A, B, C, R](recv, name)(f)

  def byName1_1_LS[B: ClassTag, C: ClassTag, R](name: String)(fast: (A, C) => Option[R])(
      f: Op11[B, C, R]
  ): CallRule =
    ruleRecv1_1ByName_LS[A, B, C, R](recv, name)(fast)(f)

  def byName1_1_LS[B: ClassTag, C: ClassTag, R](name: String)(f: Op11[B, C, R]): CallRule =
    ruleRecv1_1ByName_LS[A, B, C, R](recv, name)(f)

  def byName1_1_LSs[B: ClassTag, C: ClassTag](
      pairs: (String, (A, C) => Option[Any], Op11[B, C, Any])*
  ): List[CallRule] =
    byName1_1_LSPairs(pairs)

  def byName1_1_LSsList[B: ClassTag, C: ClassTag](
      pairs: Seq[(String, (A, C) => Option[Any], Op11[B, C, Any])]
  ): List[CallRule] =
    byName1_1_LSs(pairs*)

  def byName1_1_LSsAny(
      pairs: (String, (A, Any) => Option[Any], (A, () => Any, Any) => Any)*
  ): List[CallRule] =
    byName1_1_LSs[Any, Any](pairs*)

  def byName1_1_LSsAnyList(
      pairs: Seq[(String, (A, Any) => Option[Any], (A, () => Any, Any) => Any)]
  ): List[CallRule] =
    byName1_1_LSsAny(pairs*)

  def byName1_1_SL[B: ClassTag, C: ClassTag, R](name: String)(fast: (A, B) => Option[R])(
      f: Op2[B, C, R]
  ): CallRule =
    ruleRecv1_1ByName_SL[A, B, C, R](recv, name)(fast)(f)

  def byName1_1_SL[B: ClassTag, C: ClassTag, R](name: String)(f: Op2[B, C, R]): CallRule =
    ruleRecv1_1ByName_SL[A, B, C, R](recv, name)(f)

  def byName1_1_SLs[B: ClassTag, C: ClassTag](pairs: (String, (A, B) => Option[Any], Op2[B, C, Any])*): List[CallRule] =
    byName1_1_SLPairs(pairs)

  def byName1_1_SLsList[B: ClassTag, C: ClassTag](
      pairs: Seq[(String, (A, B) => Option[Any], Op2[B, C, Any])]
  ): List[CallRule] =
    byName1_1_SLs(pairs*)

  def byName1_1_SLsAny(
      pairs: (String, (A, Any) => Option[Any], (A, Any, () => Any) => Any)*
  ): List[CallRule] =
    byName1_1_SLs[Any, Any](pairs*)

  def byName1_1_SLsAnyList(
      pairs: Seq[(String, (A, Any) => Option[Any], (A, Any, () => Any) => Any)]
  ): List[CallRule] =
    byName1_1_SLsAny(pairs*)

  def byName_SLL[B: ClassTag, C: ClassTag, D: ClassTag, R](name: String)(fast: (A, B) => Option[R])(
      f: Op23[B, C, D, R]
  ): CallRule =
    ruleRecvByName_SLL[A, B, C, D, R](recv, name)(fast)(f)

  def byName_SLL[B: ClassTag, C: ClassTag, D: ClassTag, R](name: String)(f: Op23[B, C, D, R]): CallRule =
    ruleRecvByName_SLL[A, B, C, D, R](recv, name)(f)

  def byName_SLLs[B: ClassTag, C: ClassTag, D: ClassTag](
      pairs: (String, (A, B) => Option[Any], Op23[B, C, D, Any])*
  ): List[CallRule] =
    byName_SLLPairs(pairs)

  def byName_SLLsList[B: ClassTag, C: ClassTag, D: ClassTag](
      pairs: Seq[(String, (A, B) => Option[Any], Op23[B, C, D, Any])]
  ): List[CallRule] =
    byName_SLLs(pairs*)

  def byName_SLLsAny(
      pairs: (String, (A, Any) => Option[Any], (A, Any, () => Any, () => Any) => Any)*
  ): List[CallRule] =
    byName_SLLs[Any, Any, Any](pairs*)

  def byName_SLLsAnyList(
      pairs: Seq[(String, (A, Any) => Option[Any], (A, Any, () => Any, () => Any) => Any)]
  ): List[CallRule] =
    byName_SLLsAny(pairs*)

  def byNameCond_SL_Any(pairs: (String, (A, Boolean, () => Any) => Any)*): List[CallRule] =
    byNameCond_SL_Pairs(pairs)

  def byNameCond_SL_AnyList(pairs: Seq[(String, (A, Boolean, () => Any) => Any)]): List[CallRule] =
    byNameCond_SL_Any(pairs*)

  def byNameCond_SLL_Any(pairs: (String, (A, Boolean, () => Any, () => Any) => Any)*): List[CallRule] =
    byNameCond_SLL_Pairs(pairs)

  def byNameCond_SLL_AnyList(pairs: Seq[(String, (A, Boolean, () => Any, () => Any) => Any)]): List[CallRule] =
    byNameCond_SLL_Any(pairs*)

// === RulesFor entrypoint ===
private[comptime] final case class RulesFor[A: ClassTag](recv: RecvPred) extends RulesForOps[A], RulesForByName[A]:
  def rule[R](name: String)(f: A => R): CallRule =
    rule0(name)(f)
  def rule[B: ClassTag, R](name: String)(f: (A, B) => R): CallRule =
    rule1[B, R](name)(f)
  def rule[B: ClassTag, C: ClassTag, R](name: String)(f: (A, B, C) => R): CallRule =
    rule2[B, C, R](name)(f)
  def rule[B: ClassTag, C: ClassTag, D: ClassTag, R](name: String)(f: (A, B, C, D) => R): CallRule =
    rule3[B, C, D, R](name)(f)
  def rulePf1[R](name: String)(f: (A, PartialFunction[Any, Any]) => R): CallRule =
    rule1[Any => Any, R](name)((a, g) => f(a, g.asInstanceOf[PartialFunction[Any, Any]]))
  def rulePf1s(pairs: (String, (A, PartialFunction[Any, Any]) => Any)*): List[CallRule] =
    pairs.toList.map { case (name, f) => rulePf1[Any](name)(f) }
  def const[R](name: String)(value: => R): CallRule =
    rule0Recv(recv, name)(value)
  def consts(pairs: (String, Any)*): List[CallRule] =
    pairs.toList.map { case (name, value) => const(name)(value) }
  def constAnyArity[R](name: String)(value: => R): CallRule =
    rule0RecvAnyArity(recv, name)(value)
  def constAnyAritys(pairs: (String, Any)*): List[CallRule] =
    pairs.toList.map { case (name, value) => constAnyArity(name)(value) }
  def rule0AnyArity[R](name: String)(f: A => R): CallRule =
    ruleRecv0AnyArity[A, R](recv, name)(f)
  def rule0AnyAritys(pairs: (String, A => Any)*): List[CallRule] =
    pairs.toList.map { case (name, f) => rule0AnyArity[Any](name)(f) }
  def rule1AnyArity[B: ClassTag, R](name: String)(f: (A, B) => R): CallRule =
    ruleRecv1AnyArity[A, B, R](recv, name)(f)
  def rule0[R](name: String)(f: A => R): CallRule =
    ruleRecv0[A, R](recv, name)(f)
  def rule1[B: ClassTag, R](name: String)(f: (A, B) => R): CallRule =
    ruleRecv1[A, B, R](recv, name)(f)
  def rule2[B: ClassTag, C: ClassTag, R](name: String)(f: (A, B, C) => R): CallRule =
    ruleRecv2[A, B, C, R](recv, name)(f)
  def rule3[B: ClassTag, C: ClassTag, D: ClassTag, R](name: String)(f: (A, B, C, D) => R): CallRule =
    ruleRecv3[A, B, C, D, R](recv, name)(f)
  def rule1_1[B: ClassTag, C: ClassTag, R](name: String)(f: (A, B, C) => R): CallRule =
    ruleRecv1_1[A, B, C, R](recv, name)(f)
  def rule1_1Or2[B: ClassTag, C: ClassTag, R](name: String)(f: (A, B, C) => R): CallRule =
    RuleHelpersCore.ruleRecv1_1Or2[A, B, C, R](recv, name)(f)
  def rule1_1_1[B: ClassTag, C: ClassTag, D: ClassTag, R](name: String)(f: (A, B, C, D) => R): CallRule =
    ruleRecv1_1_1[A, B, C, D, R](recv, name)(f)
  def rule111or3[B: ClassTag, C: ClassTag, D: ClassTag, R](name: String)(f: (A, B, C, D) => R): CallRule =
    RuleHelpersCore.ruleRecv1_1_1Or3[A, B, C, D, R](recv, name)(f)

// === Typed constructors ===
private[comptime] object RulesFor:
  def any(recv: RecvPred): RulesFor[Any]                    = RulesFor[Any](recv)
  def string: RulesFor[String]                              = RulesFor[String](RuleDsl.string)
  def option(recv: RecvPred): RulesFor[Option[Any]]         = RulesFor[Option[Any]](recv)
  def either(recv: RecvPred): RulesFor[Either[Any, Any]]    = RulesFor[Either[Any, Any]](recv)
  def tryAny(recv: RecvPred): RulesFor[scala.util.Try[Any]] = RulesFor[scala.util.Try[Any]](recv)
  def iterable(recv: RecvPred): RulesFor[Iterable[Any]]     = RulesFor[Iterable[Any]](recv)
  def iterator(recv: RecvPred): RulesFor[Iterator[Any]]     = RulesFor[Iterator[Any]](recv)
  def seq(recv: RecvPred): RulesFor[Seq[Any]]               = RulesFor[Seq[Any]](recv)
  def set(recv: RecvPred): RulesFor[Set[Any]]               = RulesFor[Set[Any]](recv)
  def map(recv: RecvPred): RulesFor[Map[Any, Any]]          = RulesFor[Map[Any, Any]](recv)
