// Stdlib rules (hand-maintained).
package comptime

import RuleHelpers.*
import StdlibCollectionCommon.mapFlatMapOps

private[comptime] object StdlibEitherTables:
  val swap: List[(String, Either[Any, Any] => Any)] = List(
    "swap" -> (_.swap)
  )

  val cond: List[(String, (Any, Boolean, () => Any, () => Any) => Any)] = List(
    "cond" -> ((_: Any, cond: Boolean, right: () => Any, left: () => Any) =>
      if cond then Right(right()) else Left(left())
    )
  )

  val getOrElse: List[(String, (Either[Any, Any], () => Any) => Any)] = List(
    "getOrElse" -> ((e, fallback) => e.fold(_ => fallback(), identity))
  )

  val orElse: List[(String, (Either[Any, Any], () => Either[Any, Any]) => Any)] = List(
    "orElse" -> ((e, fallback) =>
      e match
        case Right(_) => e
        case Left(_)  => fallback()
    )
  )

  val fold: List[(String, (Either[Any, Any], (Any => Any), (Any => Any)) => Any)] = List(
    "fold" -> ((e, fl, fr) => e.fold(fl, fr))
  )

  val mapLeft: List[(String, (Either[Any, Any], (Any => Any)) => Any)] = List(
    "mapLeft" -> ((e, f) => e.left.map(v => f(v)))
  )

  // filterOrElse: Right(x) with pred(x) => Right(x), Right(x) with !pred(x) => Left(zero), Left(x) => Left(x)
  val filterOrElse: List[(String, (Either[Any, Any], Any => Boolean, () => Any) => Any)] = List(
    "filterOrElse" -> ((e, pred, zero) =>
      e match
        case Right(x) => if pred(x) then Right(x) else Left(zero())
        case Left(_)  => e
    )
  )

  // contains: Right(x) with x == elem => true, otherwise false
  val contains: List[(String, (Either[Any, Any], Any) => Any)] = List(
    "contains" -> ((e, elem) =>
      e match
        case Right(x) => x == elem
        case Left(_)  => false
    )
  )

  // forall: Right(x) => pred(x), Left(_) => true
  val forall: List[(String, (Either[Any, Any], Any => Boolean) => Any)] = List(
    "forall" -> ((e, pred) =>
      e match
        case Right(x) => pred(x)
        case Left(_)  => true
    )
  )

  // exists: Right(x) => pred(x), Left(_) => false
  val exists: List[(String, (Either[Any, Any], Any => Boolean) => Any)] = List(
    "exists" -> ((e, pred) =>
      e match
        case Right(x) => pred(x)
        case Left(_)  => false
    )
  )

  // toOption: Right(x) => Some(x), Left(_) => None
  val toOption: List[(String, Either[Any, Any] => Any)] = List(
    "toOption" -> (e =>
      e match
        case Right(x) => Some(x)
        case Left(_)  => None
    )
  )

  // toSeq: Right(x) => Seq(x), Left(_) => Seq.empty
  val toSeq: List[(String, Either[Any, Any] => Any)] = List(
    "toSeq" -> (e =>
      e match
        case Right(x) => Seq(x)
        case Left(_)  => Seq.empty
    )
  )

  // merge: extract the value from either side (both must be same type)
  val merge: List[(String, Either[Any, Any] => Any)] = List(
    "merge" -> (e =>
      e match
        case Right(x) => x
        case Left(x)  => x
    )
  )

  // joinRight: Right(Right(x)) => Right(x), Right(Left(x)) => Left(x), Left(x) => Left(x)
  val joinRight: List[(String, Either[Any, Any] => Any)] = List(
    "joinRight" -> (e =>
      e match
        case Right(inner: Either[Any, Any] @unchecked) => inner
        case Right(other)                              => Right(other) // shouldn't happen in valid joinRight call
        case Left(x)                                   => Left(x)
    )
  )

  // joinLeft: Left(Left(x)) => Left(x), Left(Right(x)) => Right(x), Right(x) => Right(x)
  val joinLeft: List[(String, Either[Any, Any] => Any)] = List(
    "joinLeft" -> (e =>
      e match
        case Left(inner: Either[Any, Any] @unchecked) => inner
        case Left(other)                              => Left(other) // shouldn't happen in valid joinLeft call
        case Right(x)                                 => Right(x)
    )
  )

  // left: get LeftProjection
  val left: List[(String, Either[Any, Any] => Any)] = List(
    "left" -> (e => e.left)
  )

private[comptime] object StdlibEitherCtorRules:
  def right(rule: RulesFor[Any]): List[CallRule] =
    rule.arg1sAny(
      "apply" -> Right.apply
    )

  def left(rule: RulesFor[Any]): List[CallRule] =
    rule.arg1sAny(
      "apply" -> Left.apply
    )

// LeftProjection tables
private[comptime] object StdlibLeftProjectionTables:
  // LeftProjection.map: Left(x) => Left(f(x)), Right(x) => Right(x)
  val map: List[(String, (Either.LeftProjection[Any, Any], Any => Any) => Any)] = List(
    "map" -> ((proj, f) =>
      proj.e match
        case Left(x)  => Left(f(x))
        case Right(x) => Right(x)
    )
  )

object StdlibEitherRules:
  private val rightRecv           = Recv.module("scala.util.Right")
  private val leftRecv            = Recv.module("scala.util.Left")
  private val eitherValueRecv     = Recv.modules("scala.util.Either", "scala.util.Left", "scala.util.Right")
  private val leftProjectionRecv  = Recv.modules("scala.util.Either.LeftProjection", "scala.util.Either$.LeftProjection")
  private val rightRule           = RulesFor.any(rightRecv)
  private val leftRule            = RulesFor.any(leftRecv)
  private val eitherValueRule     = RulesFor.either(eitherValueRecv)
  private val leftProjectionRule  = RulesFor[Either.LeftProjection[Any, Any]](leftProjectionRecv)
  private val eitherCompanionRule = RulesFor.any(Recv.moduleOnly("scala.util.Either"))

  private val eitherCtorRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibEitherCtorRules.right(rightRule),
      StdlibEitherCtorRules.left(leftRule)
    )

  private def eitherMapLike(rule: RulesFor[Either[Any, Any]]): List[CallRule] =
    RuleHelpers.concat(
      mapFlatMapOps[Either[Any, Any], Either[Any, Any]](rule)(
        (e, f) => e.map(f),
        (e, f) => e.flatMap(f)
      ),
      rule.ops1List(StdlibEitherTables.mapLeft)
    )

  // filterOrElse uses byName_SL pattern: (A, predicate, () => zero) => R
  private def eitherFilterOrElse(rule: RulesFor[Either[Any, Any]]): List[CallRule] =
    List(
      rule.byName_SL[Any => Boolean, Any, Any]("filterOrElse") { (e, pred) =>
        // Fast path: if Left, we can skip evaluating zero
        e match
          case Left(_)  => Some(e)
          case Right(x) => if pred(x) then Some(Right(x)) else None
      } { (e, pred, zero) =>
        e match
          case Right(x) => if pred(x) then Right(x) else Left(zero())
          case Left(_)  => e
      }
    )

  val rules: List[CallRule] =
    RuleHelpers.concat(
      eitherCtorRules,
      eitherValueRule.opsList(StdlibEitherTables.swap),
      eitherCompanionRule.byNameCond_SLL_AnyList(StdlibEitherTables.cond),
      eitherValueRule.byName_LsAnyList(StdlibEitherTables.getOrElse),
      eitherValueRule.byName_LsList[Either[Any, Any]](StdlibEitherTables.orElse),
      eitherValueRule.ops2List(StdlibEitherTables.fold),
      eitherFilterOrElse(eitherValueRule),
      eitherValueRule.ops1AnyList(StdlibEitherTables.contains),
      eitherValueRule.ops1PredList(StdlibEitherTables.forall),
      eitherValueRule.ops1PredList(StdlibEitherTables.exists),
      eitherValueRule.opsList(StdlibEitherTables.toOption),
      eitherValueRule.opsList(StdlibEitherTables.toSeq),
      eitherValueRule.anyArityOpsList(StdlibEitherTables.merge),
      eitherValueRule.anyArityOpsList(StdlibEitherTables.joinRight),
      eitherValueRule.anyArityOpsList(StdlibEitherTables.joinLeft),
      eitherValueRule.opsList(StdlibEitherTables.left),
      leftProjectionRule.ops1FnList(StdlibLeftProjectionTables.map),
      eitherMapLike(eitherValueRule),
      mergeableEitherRules
    )

  // MergeableEither extension method support for merge
  // The call pattern is: Either.MergeableEither(e).merge
  // 1. MergeableEither takes the Either as arg and just passes it through (value class)
  // 2. merge is then called on that result (arity 0)
  private lazy val mergeableEitherRecv =
    Recv.modules("scala.util.Either.MergeableEither", "scala.util.Either$.MergeableEither")
  private lazy val mergeableEitherRule = RulesFor.either(mergeableEitherRecv)
  private lazy val mergeableEitherRules: List[CallRule] =
    RuleHelpers.concat(
      // Handle MergeableEither(e) - just pass through the Either
      eitherCompanionRule.arg1sAny(
        "MergeableEither" -> ((e: Any) => e)
      ),
      // Handle .merge on MergeableEither result (which is just an Either)
      // Use anyArityOpsList because the receiver is a nested call (MergeableEither(e))
      mergeableEitherRule.anyArityOpsList(StdlibEitherTables.merge)
    )
