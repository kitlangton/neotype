// Stdlib rules (hand-maintained).
package comptime

import StdlibCollectionCommon.*

private[comptime] object StdlibOptionTables:
  val iterables: List[(String, Option[Any] => Any)] = List(
    "iterator" -> (_.iterator)
  )

  val containsOps: List[(String, (Option[Any], Any) => Any)] = List(
    "contains" -> ((opt, value) => opt.contains(value))
  )

  val zipOps: List[(String, (Option[Any], Option[Any]) => Any)] = List(
    "zip" -> ((opt1, opt2) => opt1.zip(opt2))
  )

  // These are companion object methods, so receiver is Any (the Option$ module)
  val whenUnless: List[(String, (Any, Boolean, () => Any) => Any)] = List(
    "when"   -> ((_, cond, body) => if cond then Some(body()) else None),
    "unless" -> ((_, cond, body) => if !cond then Some(body()) else None)
  )

  val getOrElseEither: List[(String, (Option[Any], () => Any) => Any)] = List(
    "getOrElse" -> ((opt, fallback) => opt.getOrElse(fallback())),
    "toRight"   -> ((opt, left) => opt.toRight(left())),
    "toLeft"    -> ((opt, right) => opt.toLeft(right()))
  )

  val orElse: List[(String, (Option[Any], () => Option[Any]) => Any)] = List(
    "orElse" -> ((opt, fallback) => opt.orElse(fallback()))
  )

  // fold: first arg is by-name (ifEmpty), second is function (f)
  // Fast path: if Some, we can skip ifEmpty evaluation
  def foldImpl(opt: Option[Any], ifEmpty: () => Any, f: Any => Any): Any =
    opt match
      case Some(v) => f(v)
      case None    => ifEmpty()

  val nullish: List[(String, Option[Any] => Any)] = List(
    "flatten" -> (opt =>
      opt match
        case Some(inner: Option[?]) => inner.asInstanceOf[Option[Any]]
        case None                   => None
        case _                      => throw new RuntimeException("Option.flatten on non-Option")
    ),
    "orNull" -> (_.orNull)
  )

  // sum/product for Option - dispatches based on element type
  val sumOps: List[(String, Option[Any] => Any)] = List(
    "sum" -> { opt =>
      opt match
        case Some(v: Int)        => v
        case Some(v: Long)       => v
        case Some(v: Double)     => v
        case Some(v: Float)      => v
        case Some(v: BigInt)     => v
        case Some(v: BigDecimal) => v
        case None                =>
          // Need to return zero, but we don't know the type - default to Int 0
          // In practice, the caller's expected type determines this
          0
        case Some(other) => throw new RuntimeException(s"Unsupported element type for sum: ${other.getClass}")
    },
    "product" -> { opt =>
      opt match
        case Some(v: Int)        => v
        case Some(v: Long)       => v
        case Some(v: Double)     => v
        case Some(v: Float)      => v
        case Some(v: BigInt)     => v
        case Some(v: BigDecimal) => v
        case None                =>
          // Need to return one, but we don't know the type - default to Int 1
          1
        case Some(other) => throw new RuntimeException(s"Unsupported element type for product: ${other.getClass}")
    }
  )

private[comptime] object StdlibOptionCtorRules:
  def option(rule: RulesFor[Option[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.consts(
        "empty" -> None
      ),
      rule.arg1sAny(
        "apply" -> Option.apply
      )
    )

  def some(rule: RulesFor[Any]): List[CallRule] =
    rule.arg1sAny(
      "apply" -> Some.apply
    )

  def none(rule: RulesFor[Any]): List[CallRule] =
    rule.consts(
      "empty" -> None
    )

private[comptime] object StdlibOptionBasicRules:
  def iterables(rule: RulesFor[Option[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.opsList(StdlibOptionTables.iterables),
      toListSeqOps(rule)(_.toList, _.toSeq)
    )

  def contains(rule: RulesFor[Option[Any]]): List[CallRule] =
    rule.ops1AnyList(StdlibOptionTables.containsOps)

  def zip(rule: RulesFor[Option[Any]]): List[CallRule] =
    rule.ops1List[Option[Any]](StdlibOptionTables.zipOps)

private[comptime] object StdlibOptionByNameRules:
  // whenUnless uses Any receiver since these are companion object methods
  def whenUnless(rule: RulesFor[Any]): List[CallRule] =
    rule.byNameCond_SL_AnyList(StdlibOptionTables.whenUnless)

  def getOrElseEither(rule: RulesFor[Option[Any]]): List[CallRule] =
    rule.byName_LsAnyList(StdlibOptionTables.getOrElseEither)

  def orElse(rule: RulesFor[Option[Any]]): List[CallRule] =
    rule.byName_LsList[Option[Any]](StdlibOptionTables.orElse)

  def fold(rule: RulesFor[Option[Any]]): List[CallRule] =
    // Note: fold(ifEmpty)(f) gets flattened to A2 arity, not A1_1
    // byName_LS: by-name is FIRST arg, so signature is (A, () => B, C) => R
    List(
      rule.byName_LS[Any, Any => Any, Any]("fold")((opt, f) =>
        opt.map(f) // fast path: if Some, skip ifEmpty
      )((opt, ifEmpty, f) => StdlibOptionTables.foldImpl(opt, ifEmpty, f))
    )

private[comptime] object StdlibOptionHelpers:
  def optionNullish(rule: RulesFor[Option[Any]]): List[CallRule] =
    rule.anyArityOpsList(StdlibOptionTables.nullish)

  def optionSumProduct(rule: RulesFor[Option[Any]]): List[CallRule] =
    rule.anyArityOpsList(StdlibOptionTables.sumOps)

  def optionCollect(rule: RulesFor[Option[Any]]): List[CallRule] =
    rule.collectOps((opt, f) => opt.collect(f))

  def optionMapLike(rule: RulesFor[Option[Any]]): List[CallRule] =
    RuleHelpers.concat(
      mapLikeOps[Option[Any], Option[Any]](rule)(
        (opt, f) => opt.map(f),
        (opt, f) => opt.flatMap(f),
        (opt, f) => opt.filter(f),
        (opt, f) => opt.filterNot(f)
      ),
      // withFilter returns a lazy wrapper, but for comptime we just filter eagerly
      rule.ops1Pred(
        "withFilter" -> ((opt, f) => opt.filter(f))
      )
    )

object StdlibOptionRules:
  private val optionRecv          = Recv.module("scala.Option")
  private val someRecv            = Recv.module("scala.Some")
  private val noneRecv            = Recv.module("scala.None")
  private val optionValueRecv     = Recv.modules("scala.Option", "scala.Some", "scala.None")
  private val optionValueRule     = RulesFor.option(optionValueRecv)
  private val optionRule          = RulesFor.option(optionRecv)
  private val optionCompanionRule = RulesFor.any(Recv.moduleOnly("scala.Option"))
  private val someRule            = RulesFor.any(someRecv)
  private val noneRule            = RulesFor.any(noneRecv)

  private val optionCtorRules: List[CallRule] =
    StdlibOptionCtorRules.option(optionRule)

  private val someCtorRules: List[CallRule] =
    StdlibOptionCtorRules.some(someRule)

  private val noneCtorRules: List[CallRule] =
    StdlibOptionCtorRules.none(noneRule)

  private val optionNullishRules: List[CallRule] =
    StdlibOptionHelpers.optionNullish(optionValueRule)

  private val optionCollectRules: List[CallRule] =
    StdlibOptionHelpers.optionCollect(optionValueRule)

  val rules: List[CallRule] =
    RuleHelpers.concat(
      optionCtorRules,
      someCtorRules,
      noneCtorRules,
      optionNullishRules,
      StdlibOptionByNameRules.whenUnless(optionCompanionRule),
      StdlibOptionBasicRules.iterables(optionValueRule),
      StdlibOptionBasicRules.contains(optionValueRule),
      StdlibOptionBasicRules.zip(optionValueRule),
      StdlibOptionByNameRules.getOrElseEither(optionValueRule),
      StdlibOptionByNameRules.orElse(optionValueRule),
      StdlibOptionByNameRules.fold(optionValueRule),
      optionCollectRules,
      StdlibOptionHelpers.optionMapLike(optionValueRule),
      StdlibOptionHelpers.optionSumProduct(optionValueRule)
    )
