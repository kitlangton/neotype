package comptime

import scala.annotation.targetName
import scala.reflect.ClassTag

import RuleHelperFold.*

// === Receiver predicates ===
object Recv:
  private def moduleNames(name: String): Set[String] =
    if name.endsWith("$") then Set(name, name.dropRight(1))
    else Set(name, name + "$")
  def module(name: String): RecvPred =
    UnionRecv(moduleNames(name))
  def modules(names: String*): RecvPred =
    UnionRecv(names.flatMap(moduleNames).toSet)
  def moduleOnly(name: String): RecvPred =
    TypeRecv(if name.endsWith("$") then name else name + "$")
  def union(names: String*): RecvPred =
    UnionRecv(names.toSet)

object RuleHelperRecv:
  def recvPredOf[A: ClassTag]: RecvPred =
    val cls = summon[ClassTag[A]].runtimeClass
    if cls == classOf[String] then RuleDsl.string
    else
      PrimitiveInfo
        .recvPredFor(cls)
        .getOrElse(TypeRecv(cls.getName))

// === Constant folding helpers ===
object RuleHelperFold:
  // Numeric conversion helpers - handle Scala's implicit widening (Char → Int → Long → Float → Double)
  private def toNumeric[A](value: Any, fromNumber: Number => A, fromChar: Char => A, fallback: => A): A =
    value match
      case n: Number => fromNumber(n)
      case c: Char   => fromChar(c)
      case _         => fallback

  private[comptime] def castValue[A: ClassTag](value: Any): A =
    val cls = summon[ClassTag[A]].runtimeClass
    val result: Any =
      if cls == classOf[Byte] || cls == classOf[java.lang.Byte] then toNumeric(value, _.byteValue(), _.toByte, value)
      else if cls == classOf[Short] || cls == classOf[java.lang.Short] then
        toNumeric(value, _.shortValue(), _.toShort, value)
      else if cls == classOf[Char] || cls == classOf[java.lang.Character] then
        toNumeric(value, _.intValue().toChar, identity, value)
      else if cls == classOf[Int] || cls == classOf[java.lang.Integer] then
        toNumeric(value, _.intValue(), _.toInt, value)
      else if cls == classOf[Long] || cls == classOf[java.lang.Long] then
        toNumeric(value, _.longValue(), _.toLong, value)
      else if cls == classOf[Float] || cls == classOf[java.lang.Float] then
        toNumeric(value, _.floatValue(), _.toFloat, value)
      else if cls == classOf[Double] || cls == classOf[java.lang.Double] then
        toNumeric(value, _.doubleValue(), _.toDouble, value)
      else value
    result.asInstanceOf[A]

  def fold0[A: ClassTag, R](ctx: RuleContext, recv: Eval)(f: A => R): Eval =
    recv match
      case Eval.Value(x) if ctx.foldConstants => Eval.Value(f(castValue[A](x)))
      case _                                  => Eval.Apply1(recv, Eval.Value(()), (a: Any, _: Any) => f(castValue[A](a)))

  def fold1[A: ClassTag, B: ClassTag, R](ctx: RuleContext, recv: Eval, arg: Eval)(f: (A, B) => R): Eval =
    (recv, arg) match
      case (Eval.Value(x), Eval.Value(y)) if ctx.foldConstants => Eval.Value(f(castValue[A](x), castValue[B](y)))
      case _ =>
        Eval.Apply1(recv, arg, (a: Any, b: Any) => f(castValue[A](a), castValue[B](b)))

  def fold2[A: ClassTag, B: ClassTag, C: ClassTag, R](
      ctx: RuleContext,
      recv: Eval,
      arg1: Eval,
      arg2: Eval
  )(f: (A, B, C) => R): Eval =
    (recv, arg1, arg2) match
      case (Eval.Value(x), Eval.Value(y), Eval.Value(z)) if ctx.foldConstants =>
        Eval.Value(f(castValue[A](x), castValue[B](y), castValue[C](z)))
      case _ =>
        Eval.Apply2(
          recv,
          arg1,
          arg2,
          (a: Any, b: Any, c: Any) => f(castValue[A](a), castValue[B](b), castValue[C](c))
        )

  def fold3[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag, R](
      ctx: RuleContext,
      recv: Eval,
      arg1: Eval,
      arg2: Eval,
      arg3: Eval
  )(f: (A, B, C, D) => R): Eval =
    (recv, arg1, arg2, arg3) match
      case (Eval.Value(x), Eval.Value(y), Eval.Value(z), Eval.Value(w)) if ctx.foldConstants =>
        Eval.Value(f(castValue[A](x), castValue[B](y), castValue[C](z), castValue[D](w)))
      case _ =>
        Eval.Apply3(
          recv,
          arg1,
          arg2,
          arg3,
          (a: Any, b: Any, c: Any, d: Any) => f(castValue[A](a), castValue[B](b), castValue[C](c), castValue[D](d))
        )

// === Core rule builders (by arity) ===
object RuleHelpersCore:
  private def buildRule(recv: RecvPred, name: String, arity: Arity)(f: CallCompiler): CallRule =
    RuleDsl
      .rule(name)
      .recv(recv)
      .arity(arity)
      .compile(name)(f)

  private def buildRuleAny(recv: RecvPred, name: String)(f: CallCompiler): CallRule =
    RuleDsl
      .rule(name)
      .recv(recv)
      .anyArity
      .compile(name)(f)

  def rule0Recv[R](recv: RecvPred, name: String)(value: => R): CallRule =
    buildRule(recv, name, A0) { (call, _) =>
      if call.args0 then Right(Eval.Value(value))
      else Left(ComptimeFailure.UnsupportedArity(name, ""))
    }

  def ruleStatic1[A, R](recv: RecvPred, name: String)(f: A => R): CallRule =
    buildRule(recv, name, A1) { (call, ctx) =>
      call.args1 match
        case Some(argTerm) =>
          ctx.compileTerm(argTerm).map { argEval =>
            argEval match
              case Eval.Value(value) if ctx.foldConstants =>
                Eval.Value(f(value.asInstanceOf[A]))
              case _ =>
                Eval.BuildList(List(argEval), values => f(values.head.asInstanceOf[A]))
          }
        case None =>
          Left(ComptimeFailure.UnsupportedArity(name, ""))
    }

  def ruleStatic1Id[A](recv: RecvPred, name: String): CallRule =
    ruleStatic1[A, A](recv, name)(identity)

  def ruleStatic1s[A, R](recv: RecvPred, pairs: (String, A => R)*): List[CallRule] =
    pairs.toList.map { case (name, f) => ruleStatic1[A, R](recv, name)(f) }

  def ruleStatic1sList[A, R](recv: RecvPred, pairs: Seq[(String, A => R)]): List[CallRule] =
    ruleStatic1s[A, R](recv, pairs*)

  def ruleStatic1Ids[A](recv: RecvPred, names: String*): List[CallRule] =
    names.toList.map(name => ruleStatic1Id[A](recv, name))

  def rule0RecvAnyArity[R](recv: RecvPred, name: String)(value: => R): CallRule =
    buildRuleAny(recv, name) { (_, _) =>
      Right(Eval.Value(value))
    }

  def ruleRecv0[A: ClassTag, R](recv: RecvPred, name: String)(f: A => R): CallRule =
    buildRule(recv, name, A0) { (call, ctx) =>
      call.compileRecv0(ctx, name) { recvEval =>
        fold0(ctx, recvEval)(f)
      }
    }

  // Untyped version that accepts Any => Any directly (no ClassTag, no type conversion)
  // Use this when the function already handles type conversion internally
  def ruleRecv0Untyped[R](recv: RecvPred, name: String)(f: Any => R): CallRule =
    buildRule(recv, name, A0) { (call, ctx) =>
      call.compileRecv0(ctx, name) { recvEval =>
        recvEval match
          case Eval.Value(x) if ctx.foldConstants => Eval.Value(f(x))
          case _                                  => Eval.Apply1(recvEval, Eval.Value(()), (a: Any, _: Any) => f(a))
      }
    }

  def ruleRecv0AnyArity[A: ClassTag, R](recv: RecvPred, name: String)(f: A => R): CallRule =
    buildRuleAny(recv, name) { (call, ctx) =>
      ctx.compileTerm(call.recv).map { recvEval =>
        fold0(ctx, recvEval)(f)
      }
    }

  def ruleRecv1[A: ClassTag, B: ClassTag, R](recv: RecvPred, name: String)(f: (A, B) => R): CallRule =
    buildRule(recv, name, A1) { (call, ctx) =>
      call.compileRecv1(ctx, name) { (recvEval, argEval) =>
        fold1(ctx, recvEval, argEval)(f)
      }
    }

  def ruleRecv1AnyArity[A: ClassTag, B: ClassTag, R](recv: RecvPred, name: String)(f: (A, B) => R): CallRule =
    buildRule(recv, name, ASet(Set.empty)) { (call, ctx) =>
      // Try A1 first, then A1_1 or A2 (for methods with implicits like sortBy)
      call.compileRecv1(ctx, name) { (recvEval, argEval) =>
        fold1(ctx, recvEval, argEval)(f)
      } match
        case Right(v) => Right(v)
        case Left(_: ComptimeFailure.UnsupportedArity) =>
          ComptimeDebug.log(
            s"[comptime] ruleRecv1AnyArity: trying fallbacks for $name, args1_1=${call.args1_1.isDefined}, args2=${call.args2.isDefined}"
          )
          // Try extracting first arg from curried call (e.g., sortBy(f)(ord))
          call.args1_1 match
            case Some((firstArg, _)) =>
              ComptimeDebug.log(s"[comptime] ruleRecv1AnyArity: using args1_1 for $name")
              for
                recvEval <- ctx.compileTerm(call.recv)
                argEval  <- ctx.compileTerm(firstArg)
              yield fold1(ctx, recvEval, argEval)(f)
            case None =>
              // Also try A2 and take just the first arg (implicit Ordering resolved as 2nd arg)
              call.args2 match
                case Some((firstArg, _)) =>
                  ComptimeDebug.log(s"[comptime] ruleRecv1AnyArity: using args2 for $name")
                  for
                    recvEval <- ctx.compileTerm(call.recv)
                    argEval  <- ctx.compileTerm(firstArg)
                  yield fold1(ctx, recvEval, argEval)(f)
                case None =>
                  ComptimeDebug.log(s"[comptime] ruleRecv1AnyArity: no fallback for $name")
                  Left(ComptimeFailure.UnsupportedArity(name, ""))
        case Left(err) => Left(err)
    }

  def ruleRecv2[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(f: (A, B, C) => R): CallRule =
    buildRule(recv, name, A2) { (call, ctx) =>
      call.compileRecv2(ctx, name) { (recvEval, bEval, cEval) =>
        fold2(ctx, recvEval, bEval, cEval)(f)
      }
    }

  def ruleRecv1_1[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(f: (A, B, C) => R): CallRule =
    buildRule(recv, name, A1_1) { (call, ctx) =>
      call.compileRecv1_1(ctx, name) { (recvEval, bEval, cEval) =>
        fold2(ctx, recvEval, bEval, cEval)(f)
      }
    }

  def ruleRecv1_1Or2[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, B, C) => R
  ): CallRule =
    buildRule(recv, name, ASet(Set(A1_1, A2))) { (call, ctx) =>
      call.compileRecv1_1(ctx, name) { (recvEval, bEval, cEval) =>
        fold2(ctx, recvEval, bEval, cEval)(f)
      } match
        case Right(value) => Right(value)
        case Left(_: ComptimeFailure.UnsupportedArity) =>
          call.compileRecv2(ctx, name) { (recvEval, bEval, cEval) =>
            fold2(ctx, recvEval, bEval, cEval)(f)
          }
        case Left(err) => Left(err)
    }

  def ruleRecv3[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, B, C, D) => R
  ): CallRule =
    buildRule(recv, name, A3) { (call, ctx) =>
      call.compileRecv3(ctx, name) { (recvEval, bEval, cEval, dEval) =>
        fold3(ctx, recvEval, bEval, cEval, dEval)(f)
      }
    }

  def ruleRecv4[A, B, C, D, E, R](recv: RecvPred, name: String)(
      f: (A, B, C, D, E) => R
  ): CallRule =
    buildRule(recv, name, A4) { (call, ctx) =>
      call.compileRecv4(ctx, name) { (recvEval, bEval, cEval, dEval, eEval) =>
        val evals     = List(recvEval, bEval, cEval, dEval, eEval)
        val allValues = evals.collect { case Eval.Value(v) => v }
        if ctx.foldConstants && allValues.size == evals.size then
          Eval.Value(
            f(
              allValues(0).asInstanceOf[A],
              allValues(1).asInstanceOf[B],
              allValues(2).asInstanceOf[C],
              allValues(3).asInstanceOf[D],
              allValues(4).asInstanceOf[E]
            )
          )
        else
          Eval.BuildList(
            evals,
            values =>
              f(
                values(0).asInstanceOf[A],
                values(1).asInstanceOf[B],
                values(2).asInstanceOf[C],
                values(3).asInstanceOf[D],
                values(4).asInstanceOf[E]
              )
          )
      }
    }

  def ruleRecv5[A, B, C, D, E, F, R](recv: RecvPred, name: String)(
      f: (A, B, C, D, E, F) => R
  ): CallRule =
    buildRule(recv, name, A5) { (call, ctx) =>
      call.compileRecv5(ctx, name) { (recvEval, bEval, cEval, dEval, eEval, fEval) =>
        val evals     = List(recvEval, bEval, cEval, dEval, eEval, fEval)
        val allValues = evals.collect { case Eval.Value(v) => v }
        if ctx.foldConstants && allValues.size == evals.size then
          Eval.Value(
            f(
              allValues(0).asInstanceOf[A],
              allValues(1).asInstanceOf[B],
              allValues(2).asInstanceOf[C],
              allValues(3).asInstanceOf[D],
              allValues(4).asInstanceOf[E],
              allValues(5).asInstanceOf[F]
            )
          )
        else
          Eval.BuildList(
            evals,
            values =>
              f(
                values(0).asInstanceOf[A],
                values(1).asInstanceOf[B],
                values(2).asInstanceOf[C],
                values(3).asInstanceOf[D],
                values(4).asInstanceOf[E],
                values(5).asInstanceOf[F]
              )
          )
      }
    }

  def ruleRecv1_1_1[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, B, C, D) => R
  ): CallRule =
    buildRule(recv, name, A1_1_1) { (call, ctx) =>
      call.compileRecv1_1_1(ctx, name) { (recvEval, bEval, cEval, dEval) =>
        fold3(ctx, recvEval, bEval, cEval, dEval)(f)
      }
    }

  def ruleRecv1_1_1Or3[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, B, C, D) => R
  ): CallRule =
    buildRule(recv, name, ASet(Set(A1_1_1, A3))) { (call, ctx) =>
      call.compileRecv1_1_1(ctx, name) { (recvEval, bEval, cEval, dEval) =>
        fold3(ctx, recvEval, bEval, cEval, dEval)(f)
      } match
        case Right(value) => Right(value)
        case Left(_: ComptimeFailure.UnsupportedArity) =>
          call.compileRecv3(ctx, name) { (recvEval, bEval, cEval, dEval) =>
            fold3(ctx, recvEval, bEval, cEval, dEval)(f)
          }
        case Left(err) => Left(err)
    }

// === Varargs helpers ===
object RuleHelpersVarargs:
  def ruleVarargsRecv[R](recv: RecvPred, name: String)(build: List[Any] => R): CallRule =
    RuleDsl
      .rule(name)
      .recv(recv)
      .anyArity
      .compile(name) { (call, ctx) =>
        call.compileArgsFirst(ctx, name).map { evals =>
          val allValues = evals.collect { case Eval.Value(v) => v }
          if ctx.foldConstants && allValues.size == evals.size then Eval.Value(build(allValues))
          else Eval.BuildList(evals, build)
        }
      }

  def ruleVarargsRecvWithReceiver[R](recv: RecvPred, name: String)(
      build: (Any, List[Any]) => R,
      missing: => ComptimeError = ComptimeFailure.UnsupportedArity(name, "")
  ): CallRule =
    RuleDsl
      .rule(name)
      .recv(recv)
      .anyArity
      .compile(name) { (call, ctx) =>
        val (recvTermOpt, argTerms) =
          call.recv match
            case TermIR.Ref("<none>", None) =>
              call.argsList match
                case Some(recvTerm :: rest) => (Some(recvTerm), rest)
                case _                      => (None, Nil)
            case _ =>
              (Some(call.recv), call.args.flatten)
        recvTermOpt match
          case None =>
            Left(missing)
          case Some(recvTerm) =>
            for
              recvEval <- ctx.compileTerm(recvTerm)
              argEvals <- util.Args.compileArgsList(argTerms, ctx.compileTerm)
            yield
              val allValues = (recvEval :: argEvals).collect { case Eval.Value(v) => v }
              if ctx.foldConstants && allValues.size == argEvals.size + 1 then
                Eval.Value(build(allValues.head, allValues.tail))
              else Eval.BuildList(recvEval :: argEvals, values => build(values.head, values.tail))
      }

// === Constructor helpers (companions / tuples) ===
object RuleHelpersConstructors:
  def companionRules[R](
      recv: RecvPred,
      empty: => R,
      build: List[Any] => R,
      emptyAnyArity: Boolean = false
  ): List[CallRule] =
    val emptyRule =
      if emptyAnyArity then RuleHelpers.rule0RecvAnyArity(recv, "empty")(empty)
      else RuleHelpers.rule0Recv(recv, "empty")(empty)
    List(
      emptyRule,
      RuleHelpersVarargs.ruleVarargsRecv(recv, "apply")(build)
    )

  def tuple2Rule(recv: RecvPred): CallRule =
    RuleHelpers.ruleRecv2[Any, Any, Any, (Any, Any)](recv, "apply")((_, a, b) => (a, b))

  def tuple3Rule(recv: RecvPred): CallRule =
    RuleHelpers.ruleRecv3[Any, Any, Any, Any, (Any, Any, Any)](recv, "apply")((_, a, b, c) => (a, b, c))

  def tuple4Rule(recv: RecvPred): CallRule =
    RuleHelpers.ruleRecv4[Any, Any, Any, Any, Any, (Any, Any, Any, Any)](recv, "apply")((_, a, b, c, d) => (a, b, c, d))

  def tuple5Rule(recv: RecvPred): CallRule =
    RuleHelpers.ruleRecv5[Any, Any, Any, Any, Any, Any, (Any, Any, Any, Any, Any)](recv, "apply")((_, a, b, c, d, e) =>
      (a, b, c, d, e)
    )

// === By-name evaluation helpers ===
object RuleHelperByName:
  private def thunkEval(eval: Eval): Eval =
    Eval.Value(() => Eval.run(eval))

  private def thunkValue[A: ClassTag](thunk: Any): A =
    castValue[A](thunk.asInstanceOf[() => Any]())

  def foldByName1[A: ClassTag, B: ClassTag, R](ctx: RuleContext, recv: Eval, arg: Eval)(
      f: (A, () => B) => R
  ): Eval =
    (recv, arg) match
      case (Eval.Value(x), Eval.Value(y)) if ctx.foldConstants =>
        Eval.Value(f(castValue[A](x), () => castValue[B](y)))
      case _ =>
        Eval.ApplyByName1(
          recv,
          arg,
          (a: Any, thunk: () => Any) => f(castValue[A](a), () => castValue[B](thunk()))
        )

  // ═══════════════════════════════════════════════════════════════════════════
  // By-name compilation helpers
  // These handle different lazy argument positions in 2-arg and 3-arg methods.
  // Naming: compileByName{Position} where Position describes which args are lazy
  //   - After   = last arg is lazy:  (recv, strict, lazy)
  //   - Middle  = first arg is lazy: (recv, lazy, strict) -- used by fold
  //   - After2  = last 2 args lazy:  (recv, strict, lazy, lazy)
  // ═══════════════════════════════════════════════════════════════════════════

  /** 2 args: strict first, lazy second. Signature: (A, B, () => C) => R */
  def compileByName_SL[A: ClassTag, B: ClassTag, C: ClassTag, R](
      ctx: RuleContext,
      recvTerm: TermIR,
      argTerm: TermIR,
      byNameTerm: TermIR,
      fast: (A, B) => Option[R],
      f: (A, B, () => C) => R
  ): Either[ComptimeError, Eval] =
    for
      recvEval <- ctx.compileTerm(recvTerm)
      argEval  <- ctx.compileTerm(argTerm)
      byEval   <- ctx.compileTermLazy(byNameTerm)
    yield
      def applyEval: Eval =
        Eval.Apply2(
          recvEval,
          argEval,
          thunkEval(byEval),
          (x: Any, y: Any, thunk: Any) => f(castValue[A](x), castValue[B](y), () => thunkValue[C](thunk))
        )

      (recvEval, argEval) match
        case (Eval.Value(a), Eval.Value(b)) if ctx.foldConstants =>
          val aa = castValue[A](a)
          val bb = castValue[B](b)
          fast(aa, bb) match
            case Some(value) => Eval.Value(value)
            case None =>
              byEval match
                case Eval.Value(c) => Eval.Value(f(aa, bb, () => castValue[C](c)))
                case _             => applyEval
        case _ =>
          applyEval

  /** 2 args: lazy first, strict second. Signature: (A, () => B, C) => R */
  def compileByName_LS[A: ClassTag, B: ClassTag, C: ClassTag, R](
      ctx: RuleContext,
      recvTerm: TermIR,
      byNameTerm: TermIR,
      argTerm: TermIR,
      fast: (A, C) => Option[R],
      f: (A, () => B, C) => R
  ): Either[ComptimeError, Eval] =
    for
      recvEval <- ctx.compileTerm(recvTerm)
      byEval   <- ctx.compileTermLazy(byNameTerm)
      argEval  <- ctx.compileTerm(argTerm)
    yield
      def applyEval: Eval =
        Eval.Apply2(
          recvEval,
          thunkEval(byEval),
          argEval,
          (x: Any, thunk: Any, z: Any) => f(castValue[A](x), () => thunkValue[B](thunk), castValue[C](z))
        )

      (recvEval, argEval) match
        case (Eval.Value(a), Eval.Value(c)) if ctx.foldConstants =>
          val aa = castValue[A](a)
          val cc = castValue[C](c)
          fast(aa, cc) match
            case Some(value) => Eval.Value(value)
            case None =>
              byEval match
                case Eval.Value(b) => Eval.Value(f(aa, () => castValue[B](b), cc))
                case _             => applyEval
        case _ =>
          applyEval

  def compileByName_SLL[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag, R](
      ctx: RuleContext,
      recvTerm: TermIR,
      argTerm: TermIR,
      byName1Term: TermIR,
      byName2Term: TermIR,
      fast: (A, B) => Option[R],
      f: (A, B, () => C, () => D) => R
  ): Either[ComptimeError, Eval] =
    for
      recvEval <- ctx.compileTerm(recvTerm)
      argEval  <- ctx.compileTerm(argTerm)
      byEval1  <- ctx.compileTermLazy(byName1Term)
      byEval2  <- ctx.compileTermLazy(byName2Term)
    yield
      def applyEval: Eval =
        Eval.Apply3(
          recvEval,
          argEval,
          thunkEval(byEval1),
          thunkEval(byEval2),
          (x: Any, y: Any, cTh: Any, dTh: Any) =>
            f(
              castValue[A](x),
              castValue[B](y),
              () => thunkValue[C](cTh),
              () => thunkValue[D](dTh)
            )
        )

      (recvEval, argEval) match
        case (Eval.Value(a), Eval.Value(b)) if ctx.foldConstants =>
          val aa = castValue[A](a)
          val bb = castValue[B](b)
          fast(aa, bb) match
            case Some(value) => Eval.Value(value)
            case None =>
              (byEval1, byEval2) match
                case (Eval.Value(c), Eval.Value(d)) =>
                  Eval.Value(f(aa, bb, () => castValue[C](c), () => castValue[D](d)))
                case _ => applyEval
        case _ =>
          applyEval

// === By-name rule builders ===
object RuleHelpersByName:
  import RuleHelperByName.*
  import RuleHelperRecv.*

  private def withArgs[A](args: Option[A], name: String)(
      f: A => Either[ComptimeError, Eval]
  ): Either[ComptimeError, Eval] =
    args match
      case Some(value) => f(value)
      case None        => Left(ComptimeFailure.UnsupportedArity(name, ""))

  private def buildRule(recv: RecvPred, name: String, arity: Arity)(f: CallCompiler): CallRule =
    RuleDsl
      .rule(name)
      .recv(recv)
      .arity(arity)
      .compile(name)(f)

  private def ruleRecvByName_SL_impl[A: ClassTag, B: ClassTag, C: ClassTag, R](
      recv: RecvPred,
      name: String,
      arity: Arity,
      args: CallIR => Option[(TermIR, TermIR)],
      fast: (A, B) => Option[R]
  )(
      f: (A, B, () => C) => R
  ): CallRule =
    buildRule(recv, name, arity) { (call, ctx) =>
      withArgs(args(call), name) { case (bTerm, cTerm) =>
        compileByName_SL(ctx, call.recv, bTerm, cTerm, fast, f)
      }
    }

  private def ruleRecvByName_LS_impl[A: ClassTag, B: ClassTag, C: ClassTag, R](
      recv: RecvPred,
      name: String,
      arity: Arity,
      args: CallIR => Option[(TermIR, TermIR)],
      fast: (A, C) => Option[R]
  )(
      f: (A, () => B, C) => R
  ): CallRule =
    buildRule(recv, name, arity) { (call, ctx) =>
      withArgs(args(call), name) { case (bTerm, cTerm) =>
        compileByName_LS(ctx, call.recv, bTerm, cTerm, fast, f)
      }
    }

  private def ruleRecvByName_SLL_impl[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag, R](
      recv: RecvPred,
      name: String,
      arity: Arity,
      args: CallIR => Option[(TermIR, TermIR, TermIR)],
      fast: (A, B) => Option[R]
  )(
      f: (A, B, () => C, () => D) => R
  ): CallRule =
    buildRule(recv, name, arity) { (call, ctx) =>
      withArgs(args(call), name) { case (bTerm, cTerm, dTerm) =>
        compileByName_SLL(ctx, call.recv, bTerm, cTerm, dTerm, fast, f)
      }
    }

  @targetName("ruleByName1Same")
  def ruleByName1[A: ClassTag, B: ClassTag](name: String)(f: (A, () => B) => B): CallRule =
    ruleByName1Recv[A, B, B](recvPredOf[A], name)(f)

  def ruleByName1[A: ClassTag, B: ClassTag, R](name: String)(f: (A, () => B) => R): CallRule =
    ruleByName1Recv[A, B, R](recvPredOf[A], name)(f)

  def ruleByName1Recv[A: ClassTag, B: ClassTag, R](recv: RecvPred, name: String)(f: (A, () => B) => R): CallRule =
    buildRule(recv, name, A1) { (call, ctx) =>
      call.compileRecv1Lazy(ctx, name) { (recvEval, argEval) =>
        foldByName1(ctx, recvEval, argEval)(f)
      }
    }

  def ruleRecvByName_SL[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      fast: (A, B) => Option[R]
  )(
      f: (A, B, () => C) => R
  ): CallRule =
    ruleRecvByName_SL_impl(recv, name, A2, _.args2, fast)(f)

  def ruleRecvByName_SL[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, B, () => C) => R
  ): CallRule =
    ruleRecvByName_SL(recv, name)((_: A, _: B) => None)(f)

  // Like ruleRecvByName_SL but with by-name as the FIRST arg (not last)
  // Used for Option.fold(ifEmpty)(f) where ifEmpty is by-name
  def ruleRecvByName_LS[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      fast: (A, C) => Option[R]
  )(
      f: (A, () => B, C) => R
  ): CallRule =
    ruleRecvByName_LS_impl(recv, name, A2, _.args2, fast)(f)

  def ruleRecvByName_LS[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, () => B, C) => R
  ): CallRule =
    ruleRecvByName_LS(recv, name)((_: A, _: C) => None)(f)

  def ruleRecv1_1ByName_LS[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      fast: (A, C) => Option[R]
  )(
      f: (A, () => B, C) => R
  ): CallRule =
    ruleRecvByName_LS_impl(recv, name, A1_1, _.args1_1, fast)(f)

  def ruleRecv1_1ByName_LS[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, () => B, C) => R
  ): CallRule =
    ruleRecv1_1ByName_LS(recv, name)((_: A, _: C) => None)(f)

  def ruleRecv1_1ByName_SL[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      fast: (A, B) => Option[R]
  )(
      f: (A, B, () => C) => R
  ): CallRule =
    ruleRecvByName_SL_impl(recv, name, A1_1, _.args1_1, fast)(f)

  def ruleRecv1_1ByName_SL[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, B, () => C) => R
  ): CallRule =
    ruleRecv1_1ByName_SL(recv, name)((_: A, _: B) => None)(f)

  def ruleRecvByName_SLL[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag, R](recv: RecvPred, name: String)(
      fast: (A, B) => Option[R]
  )(
      f: (A, B, () => C, () => D) => R
  ): CallRule =
    ruleRecvByName_SLL_impl(recv, name, A3, _.args3, fast)(f)

  def ruleRecvByName_SLL[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, B, () => C, () => D) => R
  ): CallRule =
    ruleRecvByName_SLL(recv, name)((_: A, _: B) => None)(f)

  def ruleByName1_2[A: ClassTag, B: ClassTag, C: ClassTag, R](recv: RecvPred, name: String)(
      f: (A, () => B, () => C) => R
  ): CallRule =
    buildRule(recv, name, A1_2) { (call, ctx) =>
      withArgs(call.args1_2, name) { case (a, b, c) =>
        for
          aEval <- ctx.compileTerm(a)
          bEval <- ctx.compileTermLazy(b)
          cEval <- ctx.compileTermLazy(c)
        yield (aEval, bEval, cEval) match
          case (Eval.Value(x), Eval.Value(y), Eval.Value(z)) if ctx.foldConstants =>
            Eval.Value(f(castValue[A](x), () => castValue[B](y), () => castValue[C](z)))
          case _ =>
            Eval.Apply2(
              aEval,
              bEval,
              cEval,
              (a: Any, b: Any, c: Any) => f(castValue[A](a), () => castValue[B](b), () => castValue[C](c))
            )
      }
    }

// === Public re-exports ===
object RuleHelpers:
  type RulesFor[A] = _root_.comptime.RulesFor[A]
  val RulesFor = _root_.comptime.RulesFor
  val Recv     = _root_.comptime.Recv
  def concat(rules: List[CallRule]*): List[CallRule] =
    rules.toList.flatten
  export RuleHelpersByName.*
  export RuleHelpersConstructors.*
  export RuleHelpersCore.*
  export RuleHelpersVarargs.*
