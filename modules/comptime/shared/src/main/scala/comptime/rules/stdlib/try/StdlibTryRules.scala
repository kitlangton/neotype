// Stdlib rules (hand-maintained).
package comptime

import RuleHelpers.*
import StdlibCollectionCommon.mapFlatMapOps

private[comptime] object StdlibTryTables:
  val basics: List[(String, scala.util.Try[Any] => Any)] = List(
    "isSuccess" -> (_.isSuccess),
    "isFailure" -> (_.isFailure),
    "toOption"  -> (_.toOption),
    "toEither"  -> (_.toEither),
    "failed"    -> (_.failed)
  )

  val getOrElse: List[(String, (scala.util.Try[Any], () => Any) => Any)] = List(
    "getOrElse" -> ((t, fallback) => t.getOrElse(fallback()))
  )

  // fold(fa: Throwable => B, fb: A => B): B - takes failure fn first, then success fn
  val fold: List[(String, (scala.util.Try[Any], Throwable => Any, Any => Any) => Any)] = List(
    "fold" -> ((t, ff, sf) => t.fold(ff, sf))
  )

  // recover with a function that handles all Throwables (converted from PartialFunction)
  val recover: List[(String, (scala.util.Try[Any], Throwable => Any) => Any)] = List(
    "recover" -> ((t, f) => t.recover { case e => f(e) })
  )

  // recoverWith with a function that handles all Throwables (converted from PartialFunction)
  val recoverWith: List[(String, (scala.util.Try[Any], Throwable => scala.util.Try[Any]) => Any)] = List(
    "recoverWith" -> ((t, f) => t.recoverWith { case e => f(e) })
  )

private[comptime] object StdlibTryCtorRules:
  def applyRule(rule: RulesFor[Any]): List[CallRule] =
    rule.byName_LsAny(
      "apply" -> ((_, thunk) => scala.util.Try(thunk()))
    )

  def successRule(rule: RulesFor[Any]): List[CallRule] =
    rule.arg1sAny(
      "apply" -> scala.util.Success.apply
    )
  // Failure.apply is not supported - requires Throwable at compile time

private[comptime] object StdlibTryRules:
  private val tryRecv      = Recv.moduleOnly("scala.util.Try")
  private val successRecv  = Recv.moduleOnly("scala.util.Success")
  private val tryValueRecv = Recv.modules("scala.util.Try", "scala.util.Success", "scala.util.Failure")
  private val tryValueRule = RulesFor.tryAny(tryValueRecv)
  private val tryRule      = RulesFor.any(tryRecv)
  private val successRule  = RulesFor.any(successRecv)

  private def tryMapLike(rule: RulesFor[scala.util.Try[Any]]): List[CallRule] =
    RuleHelpers.concat(
      mapFlatMapOps[scala.util.Try[Any], scala.util.Try[Any]](rule)(
        (t, f) => t.map(f),
        (t, f) => t.flatMap(f)
      ),
      rule.ops1ThrowableFnList(StdlibTryTables.recover),
      rule.ops1List[Throwable => scala.util.Try[Any]](StdlibTryTables.recoverWith)
    )

  // transform(s: T => Try[U], f: Throwable => Try[U]): Try[U]
  private def tryTransform(rule: RulesFor[scala.util.Try[Any]]): List[CallRule] =
    List(
      rule.rule2[Any => scala.util.Try[Any], Throwable => scala.util.Try[Any], scala.util.Try[Any]]("transform") {
        (t, s, f) => t.transform(s, f)
      }
    )

  // recover/recoverWith using partial functions (for pattern matching cases)
  private def tryRecoverPf(rule: RulesFor[scala.util.Try[Any]]): List[CallRule] =
    rule.rulePf1s(
      "recover"     -> ((t, pf) => t.recover(pf.asInstanceOf[PartialFunction[Throwable, Any]])),
      "recoverWith" -> ((t, pf) => t.recoverWith(pf.asInstanceOf[PartialFunction[Throwable, scala.util.Try[Any]]]))
    )

  val rules: List[CallRule] =
    RuleHelpers.concat(
      StdlibTryCtorRules.applyRule(tryRule),
      StdlibTryCtorRules.successRule(successRule),
      tryValueRule.opsList(StdlibTryTables.basics),
      tryValueRule.byName_LsAnyList(StdlibTryTables.getOrElse),
      tryValueRule.ops2ThrowableFnList(StdlibTryTables.fold),
      tryMapLike(tryValueRule),
      tryTransform(tryValueRule),
      tryRecoverPf(tryValueRule)
    )
