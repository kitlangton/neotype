// Stdlib rules (hand-maintained).
package comptime

import scala.reflect.ClassTag

// === Runtime type class dispatch ===
// Maps from runtime Class to typeclass instances for Ordering/Numeric operations
private[comptime] object TypeClassMaps:
  import scala.collection.immutable.Map as IMap

  val orderings: IMap[Class[?], Ordering[Any]] = IMap(
    classOf[Int]                 -> Ordering[Int].asInstanceOf[Ordering[Any]],
    classOf[java.lang.Integer]   -> Ordering[Int].asInstanceOf[Ordering[Any]],
    classOf[Long]                -> Ordering[Long].asInstanceOf[Ordering[Any]],
    classOf[java.lang.Long]      -> Ordering[Long].asInstanceOf[Ordering[Any]],
    classOf[Double]              -> Ordering[Double].asInstanceOf[Ordering[Any]],
    classOf[java.lang.Double]    -> Ordering[Double].asInstanceOf[Ordering[Any]],
    classOf[Float]               -> Ordering[Float].asInstanceOf[Ordering[Any]],
    classOf[java.lang.Float]     -> Ordering[Float].asInstanceOf[Ordering[Any]],
    classOf[String]              -> Ordering[String].asInstanceOf[Ordering[Any]],
    classOf[Char]                -> Ordering[Char].asInstanceOf[Ordering[Any]],
    classOf[java.lang.Character] -> Ordering[Char].asInstanceOf[Ordering[Any]],
    classOf[Short]               -> Ordering[Short].asInstanceOf[Ordering[Any]],
    classOf[java.lang.Short]     -> Ordering[Short].asInstanceOf[Ordering[Any]],
    classOf[Byte]                -> Ordering[Byte].asInstanceOf[Ordering[Any]],
    classOf[java.lang.Byte]      -> Ordering[Byte].asInstanceOf[Ordering[Any]],
    classOf[BigInt]              -> Ordering[BigInt].asInstanceOf[Ordering[Any]],
    classOf[BigDecimal]          -> Ordering[BigDecimal].asInstanceOf[Ordering[Any]]
  )

  val numerics: IMap[Class[?], Numeric[Any]] = IMap(
    classOf[Int]               -> Numeric[Int].asInstanceOf[Numeric[Any]],
    classOf[java.lang.Integer] -> Numeric[Int].asInstanceOf[Numeric[Any]],
    classOf[Long]              -> Numeric[Long].asInstanceOf[Numeric[Any]],
    classOf[java.lang.Long]    -> Numeric[Long].asInstanceOf[Numeric[Any]],
    classOf[Double]            -> Numeric[Double].asInstanceOf[Numeric[Any]],
    classOf[java.lang.Double]  -> Numeric[Double].asInstanceOf[Numeric[Any]],
    classOf[Float]             -> Numeric[Float].asInstanceOf[Numeric[Any]],
    classOf[java.lang.Float]   -> Numeric[Float].asInstanceOf[Numeric[Any]],
    classOf[Short]             -> Numeric[Short].asInstanceOf[Numeric[Any]],
    classOf[java.lang.Short]   -> Numeric[Short].asInstanceOf[Numeric[Any]],
    classOf[Byte]              -> Numeric[Byte].asInstanceOf[Numeric[Any]],
    classOf[java.lang.Byte]    -> Numeric[Byte].asInstanceOf[Numeric[Any]],
    classOf[BigInt]            -> Numeric[BigInt].asInstanceOf[Numeric[Any]],
    classOf[BigDecimal]        -> Numeric[BigDecimal].asInstanceOf[Numeric[Any]]
  )

  def getOrdering(sample: Any, opName: String): Ordering[Any] =
    orderings.getOrElse(
      sample.getClass,
      throw new RuntimeException(s"Unsupported type for $opName: ${sample.getClass}")
    )

  def getNumeric(sample: Any, opName: String): Numeric[Any] =
    numerics.getOrElse(sample.getClass, throw new RuntimeException(s"Unsupported type for $opName: ${sample.getClass}"))

  // ClassTag support for Array operations
  val classTags: IMap[Class[?], ClassTag[Any]] = IMap(
    classOf[Int]                 -> ClassTag.Int.asInstanceOf[ClassTag[Any]],
    classOf[java.lang.Integer]   -> ClassTag.Int.asInstanceOf[ClassTag[Any]],
    classOf[Long]                -> ClassTag.Long.asInstanceOf[ClassTag[Any]],
    classOf[java.lang.Long]      -> ClassTag.Long.asInstanceOf[ClassTag[Any]],
    classOf[Double]              -> ClassTag.Double.asInstanceOf[ClassTag[Any]],
    classOf[java.lang.Double]    -> ClassTag.Double.asInstanceOf[ClassTag[Any]],
    classOf[Float]               -> ClassTag.Float.asInstanceOf[ClassTag[Any]],
    classOf[java.lang.Float]     -> ClassTag.Float.asInstanceOf[ClassTag[Any]],
    classOf[Boolean]             -> ClassTag.Boolean.asInstanceOf[ClassTag[Any]],
    classOf[java.lang.Boolean]   -> ClassTag.Boolean.asInstanceOf[ClassTag[Any]],
    classOf[Byte]                -> ClassTag.Byte.asInstanceOf[ClassTag[Any]],
    classOf[java.lang.Byte]      -> ClassTag.Byte.asInstanceOf[ClassTag[Any]],
    classOf[Short]               -> ClassTag.Short.asInstanceOf[ClassTag[Any]],
    classOf[java.lang.Short]     -> ClassTag.Short.asInstanceOf[ClassTag[Any]],
    classOf[Char]                -> ClassTag.Char.asInstanceOf[ClassTag[Any]],
    classOf[java.lang.Character] -> ClassTag.Char.asInstanceOf[ClassTag[Any]],
    classOf[String]              -> ClassTag(classOf[String]).asInstanceOf[ClassTag[Any]]
  )

  def getClassTag(sample: Any): ClassTag[Any] =
    classTags.getOrElse(sample.getClass, ClassTag.Any)

// === Tables (data-only) ===
private[comptime] object StdlibCollectionTables:
  object Iterable:
    val basics: List[(String, Iterable[Any] => Any)] = List(
      "length" -> (_.size),
      "head"   -> (_.head),
      "tail"   -> (_.tail),
      "last"   -> (_.last)
    )

    val flattenOps: List[(String, Iterable[Any] => Any)] = List(
      "flatten" -> (xs => xs.asInstanceOf[Iterable[IterableOnce[Any]]].flatten)
    )

    val transposeOps: List[(String, Iterable[Any] => Any)] = List(
      "transpose" -> (xs => xs.asInstanceOf[Iterable[Iterable[Any]]].transpose)
    )

    val conversionOps: List[(String, Iterable[Any] => Any)] = List(
      "toMap" -> (xs => xs.asInstanceOf[Iterable[(Any, Any)]].toMap),
      "toSet" -> (_.toSet)
    )

    val predOps: List[(String, (Iterable[Any], Any => Boolean) => Any)] = List(
      "exists"    -> ((xs, f) => xs.exists(f)),
      "forall"    -> ((xs, f) => xs.forall(f)),
      "find"      -> ((xs, f) => xs.find(f)),
      "count"     -> ((xs, f) => xs.count(f)),
      "takeWhile" -> ((xs, f) => xs.takeWhile(f)),
      "dropWhile" -> ((xs, f) => xs.dropWhile(f)),
      "span"      -> ((xs, f) => xs.span(f)),
      "partition" -> ((xs, f) => xs.partition(f)),
      // withFilter returns a lazy wrapper, but for comptime we just filter eagerly
      "withFilter" -> ((xs, f) => xs.filter(f))
    )

    val groupByOps: List[(String, (Iterable[Any], Any => Any) => Any)] = List(
      "groupBy" -> ((xs, f) => xs.groupBy(f))
    )

    val intOps: List[(String, (Iterable[Any], Int) => Any)] = List(
      "takeRight" -> ((xs, n) => xs.takeRight(n)),
      "dropRight" -> ((xs, n) => xs.dropRight(n)),
      "splitAt"   -> ((xs, n) => xs.splitAt(n))
    )

    val sliceOps: List[(String, (Iterable[Any], Int, Int) => Any)] = List(
      "slice" -> ((xs, from, until) => xs.slice(from, until))
    )

    val zipOps: List[(String, (Iterable[Any], IterableOnce[Any]) => Any)] = List(
      "zip" -> ((xs, ys) => xs.zip(ys))
    )

    val zipIndex: List[(String, Iterable[Any] => Any)] = List(
      "zipWithIndex" -> (_.zipWithIndex)
    )

    // unzip for Iterable[(A, B)]
    val unzipOps: List[(String, Iterable[Any] => Any)] = List(
      "unzip" -> (xs => xs.asInstanceOf[Iterable[(Any, Any)]].unzip)
    )

    // unzip3 for Iterable[(A, B, C)]
    val unzip3Ops: List[(String, Iterable[Any] => Any)] = List(
      "unzip3" -> (xs => xs.asInstanceOf[Iterable[(Any, Any, Any)]].unzip3)
    )

    // max/min - uses TypeClassMaps for Ordering dispatch
    val minMaxOps: List[(String, Iterable[Any] => Any)] = List(
      "max" -> { xs =>
        xs.headOption match
          case Some(sample) => xs.max(TypeClassMaps.getOrdering(sample, "max"))
          case None         => throw new UnsupportedOperationException("empty.max")
      },
      "min" -> { xs =>
        xs.headOption match
          case Some(sample) => xs.min(TypeClassMaps.getOrdering(sample, "min"))
          case None         => throw new UnsupportedOperationException("empty.min")
      },
      "maxOption" -> { xs =>
        xs.headOption match
          case Some(sample) => xs.maxOption(TypeClassMaps.getOrdering(sample, "maxOption"))
          case None         => None
      },
      "minOption" -> { xs =>
        xs.headOption match
          case Some(sample) => xs.minOption(TypeClassMaps.getOrdering(sample, "minOption"))
          case None         => None
      }
    )

    // sum/product - uses TypeClassMaps for Numeric dispatch
    val sumOps: List[(String, Iterable[Any] => Any)] = List(
      "sum" -> { xs =>
        xs.headOption match
          case Some(sample) => xs.sum(TypeClassMaps.getNumeric(sample, "sum"))
          case None         => 0 // empty collection defaults to Int zero
      },
      "product" -> { xs =>
        xs.headOption match
          case Some(sample) => xs.product(TypeClassMaps.getNumeric(sample, "product"))
          case None         => 1 // empty collection defaults to Int one
      }
    )

  object Iterator:
    val basics: List[(String, Iterator[Any] => Any)] = List(
      "iterator"     -> identity,
      "hasNext"      -> (_.hasNext),
      "next"         -> (_.next()),
      "toVector"     -> (_.toVector),
      "toSet"        -> (_.toSet),
      "zipWithIndex" -> (_.zipWithIndex)
    )

    val anyArityOps: List[(String, Iterator[Any] => Any)] = List(
      "toMap" -> (_.map(_.asInstanceOf[(Any, Any)]).toMap)
    )

    val predOps: List[(String, (Iterator[Any], Any => Boolean) => Any)] = List(
      "takeWhile" -> ((it, f) => it.takeWhile(f)),
      "dropWhile" -> ((it, f) => it.dropWhile(f))
    )

    val intOps: List[(String, (Iterator[Any], Int) => Any)] = List(
      "grouped" -> ((it, n) => it.grouped(n)),
      "sliding" -> ((it, n) => it.sliding(n))
    )

    val sliceOps: List[(String, (Iterator[Any], Int, Int) => Any)] = List(
      "slice"   -> ((it, from, until) => it.slice(from, until)),
      "sliding" -> ((it, size, step) => it.sliding(size, step))
    )

    val zipOps: List[(String, (Iterator[Any], IterableOnce[Any]) => Any)] = List(
      "zip" -> ((it, other) => it.zip(other))
    )

    val zipAllOps: List[(String, (Iterator[Any], IterableOnce[Any], Any, Any) => Any)] = List(
      "zipAll" -> ((it, other, a, b) => it.zipAll(other, a, b))
    )

    // sum/product for Iterator - uses TypeClassMaps for Numeric dispatch
    val sumOps: List[(String, Iterator[Any] => Any)] = List(
      "sum" -> { it =>
        val xs = it.toList
        xs.headOption match
          case Some(sample) => xs.sum(TypeClassMaps.getNumeric(sample, "sum"))
          case None         => 0
      },
      "product" -> { it =>
        val xs = it.toList
        xs.headOption match
          case Some(sample) => xs.product(TypeClassMaps.getNumeric(sample, "product"))
          case None         => 1
      }
    )

    // mkString for Iterator
    val mkStringOps: List[(String, Iterator[Any] => Any)] = List(
      "mkString" -> (_.mkString)
    )

    val mkString1Ops: List[(String, (Iterator[Any], String) => Any)] = List(
      "mkString" -> ((it, sep) => it.mkString(sep))
    )

    val mkString3Ops: List[(String, (Iterator[Any], String, String, String) => Any)] = List(
      "mkString" -> ((it, start, sep, end) => it.mkString(start, sep, end))
    )

    // reduce/fold ops for Iterator
    val reduceOps: List[(String, (Iterator[Any], (Any, Any) => Any) => Any)] = List(
      "reduce"      -> ((it, f) => it.reduce(f)),
      "reduceLeft"  -> ((it, f) => it.reduceLeft(f)),
      "reduceRight" -> ((it, f) => it.reduceRight(f))
    )

    val reduceOptionOps: List[(String, (Iterator[Any], (Any, Any) => Any) => Any)] = List(
      "reduceOption"      -> ((it, f) => it.reduceOption(f)),
      "reduceLeftOption"  -> ((it, f) => it.reduceLeftOption(f)),
      "reduceRightOption" -> ((it, f) => it.reduceRightOption(f))
    )

    // predicate ops for Iterator
    val existsOps: List[(String, (Iterator[Any], Any => Boolean) => Any)] = List(
      "exists" -> ((it, f) => it.exists(f)),
      "forall" -> ((it, f) => it.forall(f)),
      "find"   -> ((it, f) => it.find(f)),
      "count"  -> ((it, f) => it.count(f))
    )

    // min/max for Iterator - uses TypeClassMaps for Ordering dispatch
    val minMaxOps: List[(String, Iterator[Any] => Any)] = List(
      "max" -> { it =>
        val xs = it.toList
        xs.headOption match
          case Some(sample) => xs.max(TypeClassMaps.getOrdering(sample, "max"))
          case None         => throw new UnsupportedOperationException("empty.max")
      },
      "min" -> { it =>
        val xs = it.toList
        xs.headOption match
          case Some(sample) => xs.min(TypeClassMaps.getOrdering(sample, "min"))
          case None         => throw new UnsupportedOperationException("empty.min")
      },
      "maxOption" -> { it =>
        val xs = it.toList
        xs.headOption match
          case Some(sample) => xs.maxOption(TypeClassMaps.getOrdering(sample, "maxOption"))
          case None         => None
      },
      "minOption" -> { it =>
        val xs = it.toList
        xs.headOption match
          case Some(sample) => xs.minOption(TypeClassMaps.getOrdering(sample, "minOption"))
          case None         => None
      }
    )

    val minByMaxByOps: List[(String, (Iterator[Any], Any => Any) => Any)] = List(
      "maxBy" -> { (it, f) =>
        val xs = it.toList
        if xs.isEmpty then throw new UnsupportedOperationException("empty.maxBy")
        // Precompute keys once to avoid double evaluation
        val pairs    = xs.map(x => (x, f(x)))
        val ordering = TypeClassMaps.getOrdering(pairs.head._2, "maxBy")
        pairs.maxBy(_._2)(ordering)._1
      },
      "minBy" -> { (it, f) =>
        val xs = it.toList
        if xs.isEmpty then throw new UnsupportedOperationException("empty.minBy")
        // Precompute keys once to avoid double evaluation
        val pairs    = xs.map(x => (x, f(x)))
        val ordering = TypeClassMaps.getOrdering(pairs.head._2, "minBy")
        pairs.minBy(_._2)(ordering)._1
      }
    )

    val minByMaxByOptionOps: List[(String, (Iterator[Any], Any => Any) => Any)] = List(
      "maxByOption" -> { (it, f) =>
        val xs = it.toList
        if xs.isEmpty then None
        else
          // Precompute keys once to avoid double evaluation
          val pairs    = xs.map(x => (x, f(x)))
          val ordering = TypeClassMaps.getOrdering(pairs.head._2, "maxByOption")
          Some(pairs.maxBy(_._2)(ordering)._1)
      },
      "minByOption" -> { (it, f) =>
        val xs = it.toList
        if xs.isEmpty then None
        else
          // Precompute keys once to avoid double evaluation
          val pairs    = xs.map(x => (x, f(x)))
          val ordering = TypeClassMaps.getOrdering(pairs.head._2, "minByOption")
          Some(pairs.minBy(_._2)(ordering)._1)
      }
    )

  object Seq:
    val basics: List[(String, Seq[Any] => Any)] = List(
      "reverse"    -> (_.reverse),
      "distinct"   -> (_.distinct),
      "headOption" -> (_.headOption),
      "lastOption" -> (_.lastOption),
      "indices"    -> (_.indices)
    )

    // sorted has implicit Ordering, uses TypeClassMaps for runtime dispatch
    val sortedOps: List[(String, Seq[Any] => Any)] = List(
      "sorted" -> { xs =>
        xs.headOption match
          case Some(sample) => xs.sorted(TypeClassMaps.getOrdering(sample, "sorted"))
          case None         => xs
      }
    )

    val indexOps: List[(String, (Seq[Any], Int) => Any)] = List(
      "apply" -> ((xs, idx) => xs.apply(idx))
    )

    val updated: List[(String, (Seq[Any], Int, Any) => Any)] = List(
      "updated" -> ((xs, idx, value) => xs.updated(idx, value))
    )

    val sortWithOps: List[(String, (Seq[Any], (Any, Any) => Boolean) => Any)] = List(
      "sortWith" -> ((xs, lt) => xs.sortWith(lt))
    )

    val sortByOps: List[(String, (Seq[Any], Any => Any) => Any)] = List(
      "sortBy" -> { (xs, f) =>
        if xs.isEmpty then xs
        else
          // Precompute keys once to avoid double evaluation
          val pairs    = xs.map(x => (x, f(x)))
          val ordering = TypeClassMaps.getOrdering(pairs.head._2, "sortBy")
          pairs.sortBy(_._2)(ordering).map(_._1)
      }
    )

    val distinctByOps: List[(String, (Seq[Any], Any => Any) => Any)] = List(
      "distinctBy" -> ((xs, f) => xs.distinctBy(f))
    )

    // sameElements takes an IterableOnce argument
    val sameElementsOps: List[(String, (Seq[Any], IterableOnce[Any]) => Any)] = List(
      "sameElements" -> ((xs, other) => xs.sameElements(other))
    )

    // indexOf / lastIndexOf with single element argument
    val indexOfOps: List[(String, (Seq[Any], Any) => Any)] = List(
      "indexOf"     -> ((xs, elem) => xs.indexOf(elem)),
      "lastIndexOf" -> ((xs, elem) => xs.lastIndexOf(elem))
    )

    // indexOf / lastIndexOf with element and from/end index
    val indexOfFromOps: List[(String, (Seq[Any], Any, Int) => Any)] = List(
      "indexOf"     -> ((xs, elem, from) => xs.indexOf(elem, from)),
      "lastIndexOf" -> ((xs, elem, end) => xs.lastIndexOf(elem, end))
    )

    // indexWhere / lastIndexWhere with predicate only
    val indexWhereOps: List[(String, (Seq[Any], Any => Boolean) => Any)] = List(
      "indexWhere"     -> ((xs, p) => xs.indexWhere(p)),
      "lastIndexWhere" -> ((xs, p) => xs.lastIndexWhere(p))
    )

    // indexWhere / lastIndexWhere with predicate and from/end index
    val indexWhereFromOps: List[(String, (Seq[Any], Any => Boolean, Int) => Any)] = List(
      "indexWhere"     -> ((xs, p, from) => xs.indexWhere(p, from)),
      "lastIndexWhere" -> ((xs, p, end) => xs.lastIndexWhere(p, end))
    )

    // patch(from, other, replaced)
    val patchOps: List[(String, (Seq[Any], Int, IterableOnce[Any], Int) => Any)] = List(
      "patch" -> ((xs, from, other, replaced) => xs.patch(from, other, replaced))
    )

    // padTo(len, elem)
    val padToOps: List[(String, (Seq[Any], Int, Any) => Any)] = List(
      "padTo" -> ((xs, len, elem) => xs.padTo(len, elem))
    )

  object ArrayOps:
    val indexOps: List[(String, (Array[Any], Int) => Any)] = List(
      "apply" -> ((arr, idx) => arr.apply(idx))
    )

  object Map:
    val keyOps: List[(String, (Map[Any, Any], Any) => Any)] = List(
      "get"     -> ((m, key) => m.get(key)),
      "apply"   -> ((m, key) => m.apply(key)),
      "removed" -> ((m, key) => m.removed(key))
    )

    val updated: List[(String, (Map[Any, Any], Any, Any) => Any)] = List(
      "updated" -> ((m, key, value) => m.updated(key, value))
    )

    // Key/value accessors - return Iterable or Set
    val accessorOps: List[(String, Map[Any, Any] => Any)] = List(
      "keys"   -> (_.keys),
      "values" -> (_.values),
      "keySet" -> (_.keySet)
    )

    // filterKeys: filter by key predicate (via view) - deprecated but useful
    val filterKeysOps: List[(String, (Map[Any, Any], Any => Boolean) => Any)] = List(
      "filterKeys" -> ((m, p) => m.view.filterKeys(p).toMap)
    )

    // mapValues: transform values (via view) - deprecated but useful
    val mapValuesOps: List[(String, (Map[Any, Any], Any => Any) => Any)] = List(
      "mapValues" -> ((m, f) => m.view.mapValues(f).toMap)
    )

    // transform: apply function (k, v) => newV to each entry
    val transformOps: List[(String, (Map[Any, Any], (Any, Any) => Any) => Any)] = List(
      "transform" -> ((m, f) => m.transform(f))
    )

    // filter/filterNot for Map[K, V] with predicate on tuple
    val filterOps: List[(String, (Map[Any, Any], Any => Boolean) => Any)] = List(
      "filter"    -> ((m, p) => m.filter(e => p(e))),
      "filterNot" -> ((m, p) => m.filterNot(e => p(e)))
    )

    // map for Map[K, V] - f: ((K, V)) => (K2, V2)
    val mapOps: List[(String, (Map[Any, Any], Any => Any) => Any)] = List(
      "map" -> ((m, f) => m.map(e => f(e).asInstanceOf[(Any, Any)]))
    )

    // flatMap for Map[K, V] - f: ((K, V)) => IterableOnce[(K2, V2)]
    val flatMapOps: List[(String, (Map[Any, Any], Any => IterableOnce[(Any, Any)]) => Any)] = List(
      "flatMap" -> ((m, f) => m.flatMap(e => f(e)))
    )

// === Shared helper functions ===
private[comptime] object StdlibCollectionCommon:
  import RuleHelpers.*

  def sizeBasics[A](rule: RulesFor[A])(size: A => Int, isEmpty: A => Boolean, nonEmpty: A => Boolean): List[CallRule] =
    rule.ops(
      "size"     -> size,
      "isEmpty"  -> isEmpty,
      "nonEmpty" -> nonEmpty
    )

  def tupleRules(
      tuple2Recv: RecvPred,
      tuple3Recv: RecvPred,
      tuple4Recv: RecvPred,
      tuple5Recv: RecvPred
  ): List[CallRule] =
    tuple2Rule(tuple2Recv) :: tuple3Rule(tuple3Recv) :: tuple4Rule(tuple4Recv) :: tuple5Rule(tuple5Recv) :: Nil

  def mapLikeOps[A, F](
      rule: RulesFor[A]
  )(
      map: (A, Any => Any) => Any,
      flatMap: (A, Any => F) => Any,
      filter: (A, Any => Boolean) => Any,
      filterNot: (A, Any => Boolean) => Any
  ): List[CallRule] =
    RuleHelpers.concat(
      mapFlatMapOps(rule)(map, flatMap),
      rule.ops1Pred(
        "filter"    -> ((xs: A, f: Any => Boolean) => filter(xs, f)),
        "filterNot" -> ((xs: A, f: Any => Boolean) => filterNot(xs, f))
      )
    )

  def mapFlatMapOps[A, F](
      rule: RulesFor[A]
  )(
      map: (A, Any => Any) => Any,
      flatMap: (A, Any => F) => Any
  ): List[CallRule] =
    RuleHelpers.concat(
      rule.ops1Fn(
        "map" -> ((xs: A, f: Any => Any) => map(xs, f))
      ),
      rule.ops1FnOf[F](
        "flatMap" -> ((xs: A, f: Any => F) => flatMap(xs, f))
      )
    )

  def containsOps[A](rule: RulesFor[A])(f: (A, Any) => Boolean): List[CallRule] =
    rule.ops1Any(
      "contains" -> f
    )

  def toListSeqOps[A](rule: RulesFor[A])(toList: A => List[Any], toSeq: A => Seq[Any]): List[CallRule] =
    rule.ops(
      "toList" -> toList,
      "toSeq"  -> toSeq
    )

  def concatOps[A, B: scala.reflect.ClassTag](rule: RulesFor[A])(f: (A, B) => A): List[CallRule] =
    rule.ops1[B](
      "++" -> f
    )

  def takeDropOps[A, R](rule: RulesFor[A])(take: (A, Int) => R, drop: (A, Int) => R): List[CallRule] =
    rule.ops1[Int](
      "take" -> ((xs: A, n: Int) => take(xs, n)),
      "drop" -> ((xs: A, n: Int) => drop(xs, n))
    )

// === Reusable rule groups ===
private[comptime] object StdlibRuleFamily:
  import RuleHelpers.*
  import StdlibCollectionCommon.*

  def mkString(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops(
        "mkString" -> (_.mkString)
      ),
      rule.ops1[String](
        "mkString" -> ((xs, sep) => xs.mkString(sep))
      ),
      rule.ops3[String, String, String](
        "mkString" -> ((xs, start, sep, end) => xs.mkString(start, sep, end))
      )
    )

  def size(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    sizeBasics(rule)(_.size, _.isEmpty, _.nonEmpty)

  def takeDrop(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    takeDropOps(rule)((xs, n) => xs.take(n), (xs, n) => xs.drop(n))

// === Iterable rules ===
private[comptime] object StdlibIterableHelpers:
  import RuleHelpers.*
  import StdlibCollectionCommon.*

  def iterableBasics(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    RuleHelpers.concat(
      StdlibRuleFamily.size(rule),
      rule.opsList(StdlibCollectionTables.Iterable.basics),
      rule.anyArityOpsList(StdlibCollectionTables.Iterable.flattenOps),
      rule.anyArityOpsList(StdlibCollectionTables.Iterable.transposeOps),
      rule.anyArityOpsList(StdlibCollectionTables.Iterable.conversionOps),
      containsOps(rule)((xs, value) => xs.exists(_ == value)),
      rule.ops1PredList(StdlibCollectionTables.Iterable.predOps),
      rule.ops1IntList(StdlibCollectionTables.Iterable.intOps),
      rule.ops2IntIntList(StdlibCollectionTables.Iterable.sliceOps),
      rule.opsList(StdlibCollectionTables.Iterable.zipIndex),
      rule.ops1List[IterableOnce[Any]](StdlibCollectionTables.Iterable.zipOps),
      rule.anyArityOpsList(StdlibCollectionTables.Iterable.unzipOps),
      rule.anyArityOpsList(StdlibCollectionTables.Iterable.unzip3Ops),
      rule.anyArityOpsList(StdlibCollectionTables.Iterable.sumOps),
      rule.anyArityOpsList(StdlibCollectionTables.Iterable.minMaxOps),
      rule.collectOps((xs, f) => xs.collect(f)),
      rule.rulePf1s(
        "collectFirst" -> ((xs, f) => xs.collectFirst(f))
      ),
      rule.ops1FnList(StdlibCollectionTables.Iterable.groupByOps)
    )

  def iterableMapLike(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    mapLikeOps[Iterable[Any], IterableOnce[Any]](rule)(
      (xs, f) => xs.map(f),
      (xs, f) => xs.flatMap(x => f(x)),
      (xs, f) => xs.filter(f),
      (xs, f) => xs.filterNot(f)
    )

  def iterableFoldLeft(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    rule.foldLeftAny("foldLeft") { (xs, zero, op) =>
      xs.foldLeft(zero)((acc, a) => op(acc, a))
    } :: Nil

  def iterableFoldRight(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    rule.foldRightAny("foldRight") { (xs, zero, op) =>
      xs.foldRight(zero)((a, acc) => op(a, acc))
    } :: Nil

  def iterableReduce(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    List(
      rule.reduceAny("reduce") { (xs, op) =>
        xs.reduce((a, b) => op(a, b))
      },
      rule.reduceAny("reduceLeft") { (xs, op) =>
        xs.reduceLeft((a, b) => op(a, b))
      },
      rule.reduceAny("reduceRight") { (xs, op) =>
        xs.reduceRight((a, b) => op(a, b))
      },
      rule.reduceAny("reduceOption") { (xs, op) =>
        xs.reduceOption((a, b) => op(a, b))
      },
      rule.reduceAny("reduceLeftOption") { (xs, op) =>
        xs.reduceLeftOption((a, b) => op(a, b))
      },
      rule.reduceAny("reduceRightOption") { (xs, op) =>
        xs.reduceRightOption((a, b) => op(a, b))
      }
    )

  // partitionMap(f: A => Either[B, C]) - partition into two collections based on Either result
  def iterablePartitionMap(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    rule.ops1FnOf[Either[Any, Any]](
      "partitionMap" -> ((xs, f) => xs.partitionMap(x => f(x)))
    )

  // groupMap(key)(value) - curried: group by key and map values
  // Supports both curried (1)(1) and flat (2) arities
  def iterableGroupMap(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    rule.rule1_1Or2[Any => Any, Any => Any, Map[Any, Iterable[Any]]]("groupMap") { (xs, keyFn, valueFn) =>
      xs.groupMap(keyFn)(valueFn)
    } :: Nil

  // groupMapReduce(key)(value)(reduce) - triple curried: group, map, and reduce in one pass
  // Supports both curried (1)(1)(1) and flat (3) arities
  def iterableGroupMapReduce(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    rule.rule111or3[Any => Any, Any => Any, (Any, Any) => Any, Map[Any, Any]]("groupMapReduce") {
      (xs, keyFn, valueFn, reduceFn) =>
        xs.groupMapReduce(keyFn)(valueFn)((a, b) => reduceFn(a, b))
    } :: Nil

  // tapEach(f) - side-effect on each element, return original collection
  def iterableTapEach(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    rule.ops1Fn(
      "tapEach" -> ((xs, f) => xs.tapEach(f))
    )

  // scanLeft(z)(op) - like foldLeft but returns all intermediate results
  def iterableScanLeft(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    rule.rule1_1Or2[Any, (Any, Any) => Any, Iterable[Any]]("scanLeft") { (xs, zero, op) =>
      xs.scanLeft(zero)((acc, a) => op(acc, a))
    } :: Nil

  // scanRight(z)(op) - like foldRight but returns all intermediate results
  def iterableScanRight(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    rule.rule1_1Or2[Any, (Any, Any) => Any, Iterable[Any]]("scanRight") { (xs, zero, op) =>
      xs.scanRight(zero)((a, acc) => op(a, acc))
    } :: Nil

  def mkStringOps(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    StdlibRuleFamily.mkString(rule)

  def seqSliceOps(rule: RulesFor[Iterable[Any]]): List[CallRule] =
    StdlibRuleFamily.takeDrop(rule)

// === Iterator rules ===
private[comptime] object StdlibIteratorHelpers:
  import RuleHelpers.*
  import StdlibCollectionCommon.*

  def iteratorMapLike(rule: RulesFor[Iterator[Any]]): List[CallRule] =
    mapLikeOps[Iterator[Any], IterableOnce[Any]](rule)(
      (it, f) => it.map(f),
      (it, f) => it.flatMap(x => f(x)),
      (it, f) => it.filter(f),
      (it, f) => it.filterNot(f)
    )

  def iteratorBasics(rule: RulesFor[Iterator[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.opsList(StdlibCollectionTables.Iterator.basics),
      toListSeqOps(rule)(_.toList, _.toSeq)
    )

  def iteratorSliceOps(rule: RulesFor[Iterator[Any]]): List[CallRule] =
    rule.ops2IntIntList(StdlibCollectionTables.Iterator.sliceOps)

  def iteratorExtras(rule: RulesFor[Iterator[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.anyArityOpsList(StdlibCollectionTables.Iterator.anyArityOps),
      rule.anyArityOpsList(StdlibCollectionTables.Iterator.sumOps),
      rule.anyArityOpsList(StdlibCollectionTables.Iterator.minMaxOps),
      rule.anyArityOpsList(StdlibCollectionTables.Iterator.mkStringOps),
      rule.ops1List[String](StdlibCollectionTables.Iterator.mkString1Ops),
      rule.ops3List[String, String, String](StdlibCollectionTables.Iterator.mkString3Ops),
      rule.collectOps((it, f) => it.collect(f)),
      rule.ops1PredList(StdlibCollectionTables.Iterator.predOps),
      rule.ops1PredList(StdlibCollectionTables.Iterator.existsOps),
      takeDropOps(rule)((it, n) => it.take(n), (it, n) => it.drop(n)),
      rule.ops1IntList(StdlibCollectionTables.Iterator.intOps),
      rule.ops1List[IterableOnce[Any]](StdlibCollectionTables.Iterator.zipOps),
      rule.ops3List[IterableOnce[Any], Any, Any](StdlibCollectionTables.Iterator.zipAllOps),
      List(
        rule.reduceAny("reduce")((it, f) => it.reduce(f)),
        rule.reduceAny("reduceLeft")((it, f) => it.reduceLeft(f)),
        rule.reduceAny("reduceRight")((it, f) => it.reduceRight(f)),
        rule.reduceAny("reduceOption")((it, f) => it.reduceOption(f)),
        rule.reduceAny("reduceLeftOption")((it, f) => it.reduceLeftOption(f)),
        rule.reduceAny("reduceRightOption")((it, f) => it.reduceRightOption(f))
      ),
      rule.ops1FnList(StdlibCollectionTables.Iterator.minByMaxByOps),
      rule.ops1FnList(StdlibCollectionTables.Iterator.minByMaxByOptionOps)
    )

// === Seq rules ===
private[comptime] object StdlibSeqHelpers:
  import RuleHelpers.*

  def seqBasics(rule: RulesFor[Seq[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops2IntAnyList(StdlibCollectionTables.Seq.updated),
      rule.ops1IntList(StdlibCollectionTables.Seq.indexOps),
      rule.opsList(StdlibCollectionTables.Seq.basics),
      rule.anyArityOpsList(StdlibCollectionTables.Seq.sortedOps),
      seqSortOps(rule),
      seqExtraOps(rule)
    )

  def seqSortOps(rule: RulesFor[Seq[Any]]): List[CallRule] =
    RuleHelpers.concat(
      List(
        rule.sortWithAny("sortWith") { (xs, lt) =>
          xs.sortWith(lt)
        }
      ),
      // sortBy(f)(implicit ord) - curried with implicit, needs any-arity matching
      rule.anyArityOps1FnList(StdlibCollectionTables.Seq.sortByOps)
    )

  def seqExtraOps(rule: RulesFor[Seq[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops1FnList(StdlibCollectionTables.Seq.distinctByOps),
      rule.ops1List[IterableOnce[Any]](StdlibCollectionTables.Seq.sameElementsOps),
      seqCorrespondsOps(rule),
      seqIndexOps(rule)
    )

  // corresponds is curried: corresponds(other)(p) where p: (A, B) => Boolean
  def seqCorrespondsOps(rule: RulesFor[Seq[Any]]): List[CallRule] =
    List(
      rule.rule1_1Or2[IterableOnce[Any], (Any, Any) => Boolean, Boolean]("corresponds") { (xs, other, p) =>
        xs.corresponds(other)(p)
      }
    )

  def seqIndexOps(rule: RulesFor[Seq[Any]]): List[CallRule] =
    RuleHelpers.concat(
      // indexOf / lastIndexOf with single element
      rule.ops1AnyList(StdlibCollectionTables.Seq.indexOfOps),
      // indexOf / lastIndexOf with element and from/end index
      rule.ops2AnyIntList(StdlibCollectionTables.Seq.indexOfFromOps),
      // indexWhere / lastIndexWhere with predicate only
      rule.ops1PredList(StdlibCollectionTables.Seq.indexWhereOps),
      // indexWhere / lastIndexWhere with predicate and from/end index
      rule.ops2List[Any => Boolean, Int](StdlibCollectionTables.Seq.indexWhereFromOps),
      // patch(from, other, replaced)
      rule.ops3List[Int, IterableOnce[Any], Int](StdlibCollectionTables.Seq.patchOps),
      // padTo(len, elem)
      rule.ops2IntAnyList(StdlibCollectionTables.Seq.padToOps)
    )

// === Set rules ===
private[comptime] object StdlibSetHelpers:
  import RuleHelpers.*
  import StdlibCollectionCommon.*

  def setBasics(rule: RulesFor[Set[Any]]): List[CallRule] =
    RuleHelpers.concat(
      sizeBasics(rule)(_.size, _.isEmpty, _.nonEmpty),
      concatOps(rule)((xs, other: Set[Any]) => xs ++ other),
      containsOps(rule)((xs, value) => xs.contains(value)),
      // Set-specific operations
      setOperations(rule),
      setElementOperations(rule),
      setMapLikeOps(rule)
    )

  // Set operations: union, intersect, diff, subsetOf
  private def setOperations(rule: RulesFor[Set[Any]]): List[CallRule] =
    RuleHelpers.concat(
      // union / | - same as ++
      rule.ops1[Set[Any]](
        "union" -> ((xs, other) => xs.union(other)),
        "|"     -> ((xs, other) => xs | other)
      ),
      // intersect / &
      rule.ops1[Set[Any]](
        "intersect" -> ((xs, other) => xs.intersect(other)),
        "&"         -> ((xs, other) => xs & other)
      ),
      // diff / --
      rule.ops1[Set[Any]](
        "diff" -> ((xs, other) => xs.diff(other)),
        "--"   -> ((xs, other) => xs -- other)
      ),
      // subsetOf
      rule.ops1[Set[Any]](
        "subsetOf" -> ((xs, other) => xs.subsetOf(other))
      )
    )

  // Element operations: +, -, incl, excl
  private def setElementOperations(rule: RulesFor[Set[Any]]): List[CallRule] =
    rule.ops1Any(
      "+"    -> ((xs, elem) => xs + elem),
      "-"    -> ((xs, elem) => xs - elem),
      "incl" -> ((xs, elem) => xs.incl(elem)),
      "excl" -> ((xs, elem) => xs.excl(elem))
    )

  // map, filter, flatMap, filterNot for Set
  private def setMapLikeOps(rule: RulesFor[Set[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops1Fn(
        "map" -> ((xs, f) => xs.map(f))
      ),
      rule.ops1FnOf[IterableOnce[Any]](
        "flatMap" -> ((xs, f) => xs.flatMap(x => f(x)))
      ),
      rule.ops1Pred(
        "filter"    -> ((xs, f) => xs.filter(f)),
        "filterNot" -> ((xs, f) => xs.filterNot(f))
      )
    )

// === Map rules ===
private[comptime] object StdlibMapHelpers:
  import RuleHelpers.*
  import StdlibCollectionCommon.*

  private def mapKeyOps(rule: RulesFor[Map[Any, Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops1AnyList(StdlibCollectionTables.Map.keyOps),
      containsOps(rule)((m, key) => m.contains(key))
    )

  // Map accessor operations: keys, values, keySet
  private def mapAccessorOps(rule: RulesFor[Map[Any, Any]]): List[CallRule] =
    rule.opsList(StdlibCollectionTables.Map.accessorOps)

  // Map transformation operations: transform
  // Note: filterKeys and mapValues are deprecated and require MapView support
  // Use filter { case (k, _) => ... } and map { case (k, v) => (k, f(v)) } instead
  private def mapTransformOps(rule: RulesFor[Map[Any, Any]]): List[CallRule] =
    // transform requires (Any, Any) => Any function
    List(
      rule.rule[(Any, Any) => Any, Any]("transform") { (m, f) =>
        m.transform(f)
      }
    )

  // Map HOF operations: filter, filterNot, map, flatMap
  private def mapHofOps(rule: RulesFor[Map[Any, Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops1PredList(StdlibCollectionTables.Map.filterOps),
      rule.ops1FnList(StdlibCollectionTables.Map.mapOps),
      // flatMap needs special handling for the return type
      rule.ops1FnOf[IterableOnce[(Any, Any)]](
        "flatMap" -> ((m, f) => m.flatMap(e => f(e)))
      )
    )

  def mapBasics(rule: RulesFor[Map[Any, Any]]): List[CallRule] =
    RuleHelpers.concat(
      sizeBasics(rule)(_.size, _.isEmpty, _.nonEmpty),
      concatOps(rule)((m, other: Map[Any, Any]) => m ++ other),
      rule.ops2AnyList(StdlibCollectionTables.Map.updated),
      mapKeyOps(rule),
      mapAccessorOps(rule),
      mapTransformOps(rule),
      mapHofOps(rule)
    )

// === Tuple2 instance rules ===
private[comptime] object StdlibTuple2Helpers:
  import RuleHelpers.*

  def tuple2Swap(rule: RulesFor[(Any, Any)]): List[CallRule] =
    rule.ops(
      "swap" -> (t => t.swap)
    )

// === LazyList rules ===
private[comptime] object StdlibLazyListHelpers:
  import RuleHelpers.*
  import StdlibCollectionCommon.*

  def lazyListBasics(rule: RulesFor[LazyList[Any]]): List[CallRule] =
    RuleHelpers.concat(
      sizeBasics(rule)(_.size, _.isEmpty, _.nonEmpty),
      rule.ops(
        "head"       -> (_.head),
        "tail"       -> (_.tail),
        "last"       -> (_.last),
        "headOption" -> (_.headOption),
        "lastOption" -> (_.lastOption),
        "reverse"    -> (_.reverse),
        "distinct"   -> (_.distinct),
        "force"      -> (_.force),
        "toList"     -> (_.toList),
        "toVector"   -> (_.toVector),
        "toSeq"      -> (_.toSeq),
        "toSet"      -> (_.toSet)
      ),
      rule.anyArityOpsList(StdlibCollectionTables.Iterable.flattenOps.map { case (name, f) =>
        (name, (xs: LazyList[Any]) => f(xs))
      }),
      containsOps(rule)((xs, value) => xs.contains(value)),
      takeDropOps(rule)((xs, n) => xs.take(n), (xs, n) => xs.drop(n)),
      rule.ops1Int(
        "takeRight" -> ((xs, n) => xs.takeRight(n)),
        "dropRight" -> ((xs, n) => xs.dropRight(n)),
        "apply"     -> ((xs, idx) => xs.apply(idx)),
        "splitAt"   -> ((xs, n) => xs.splitAt(n))
      ),
      rule.ops2IntInt(
        "slice" -> ((xs, from, until) => xs.slice(from, until))
      ),
      rule.ops2[Int, Any](
        "updated" -> ((xs, idx, value) => xs.updated(idx, value))
      )
    )

  def lazyListPredicates(rule: RulesFor[LazyList[Any]]): List[CallRule] =
    rule.ops1Pred(
      "exists"    -> ((xs, f) => xs.exists(f)),
      "forall"    -> ((xs, f) => xs.forall(f)),
      "find"      -> ((xs, f) => xs.find(f)),
      "count"     -> ((xs, f) => xs.count(f)),
      "takeWhile" -> ((xs, f) => xs.takeWhile(f)),
      "dropWhile" -> ((xs, f) => xs.dropWhile(f)),
      "filter"    -> ((xs, f) => xs.filter(f)),
      "filterNot" -> ((xs, f) => xs.filterNot(f)),
      "span"      -> ((xs, f) => xs.span(f)),
      "partition" -> ((xs, f) => xs.partition(f))
    )

  def lazyListTransforms(rule: RulesFor[LazyList[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops1Fn(
        "map"     -> ((xs, f) => xs.map(f)),
        "groupBy" -> ((xs, f) => xs.groupBy(f))
      ),
      rule.ops1FnOf[IterableOnce[Any]](
        "flatMap" -> ((xs, f) => xs.flatMap(x => f(x)))
      ),
      rule.collectOps((xs, f) => xs.collect(f))
    )

  def lazyListFolds(rule: RulesFor[LazyList[Any]]): List[CallRule] =
    List(
      rule.foldLeftAny("foldLeft") { (xs, zero, op) =>
        xs.foldLeft(zero)((acc, a) => op(acc, a))
      },
      rule.foldRightAny("foldRight") { (xs, zero, op) =>
        xs.foldRight(zero)((a, acc) => op(a, acc))
      },
      rule.reduceAny("reduce") { (xs, op) =>
        xs.reduce((a, b) => op(a, b))
      },
      rule.reduceAny("reduceLeft") { (xs, op) =>
        xs.reduceLeft((a, b) => op(a, b))
      },
      rule.reduceAny("reduceRight") { (xs, op) =>
        xs.reduceRight((a, b) => op(a, b))
      },
      rule.reduceAny("reduceOption") { (xs, op) =>
        xs.reduceOption((a, b) => op(a, b))
      },
      rule.reduceAny("reduceLeftOption") { (xs, op) =>
        xs.reduceLeftOption((a, b) => op(a, b))
      },
      rule.reduceAny("reduceRightOption") { (xs, op) =>
        xs.reduceRightOption((a, b) => op(a, b))
      }
    )

  def lazyListZipOps(rule: RulesFor[LazyList[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops(
        "zipWithIndex" -> (_.zipWithIndex)
      ),
      rule.ops1[IterableOnce[Any]](
        "zip" -> ((xs, ys) => xs.zip(ys))
      )
    )

  def lazyListNumericOps(rule: RulesFor[LazyList[Any]]): List[CallRule] =
    rule.anyArityOpsList(StdlibCollectionTables.Iterable.sumOps.map { case (name, f) =>
      (name, (xs: LazyList[Any]) => f(xs))
    }) ++
      rule.anyArityOpsList(StdlibCollectionTables.Iterable.minMaxOps.map { case (name, f) =>
        (name, (xs: LazyList[Any]) => f(xs))
      })

  def lazyListMkString(rule: RulesFor[LazyList[Any]]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops(
        "mkString" -> (_.mkString)
      ),
      rule.ops1[String](
        "mkString" -> ((xs, sep) => xs.mkString(sep))
      ),
      rule.ops3[String, String, String](
        "mkString" -> ((xs, start, sep, end) => xs.mkString(start, sep, end))
      )
    )

  def lazyListConcat(rule: RulesFor[LazyList[Any]]): List[CallRule] =
    rule.ops1[IterableOnce[Any]](
      "++" -> ((xs, other) => xs ++ other)
    )

// === Collection companions ===
private[comptime] object StdlibCollectionCompanionRules:
  def iterableFactoryApply(factoryRecv: RecvPred): CallRule =
    RuleHelpersVarargs.ruleVarargsRecvWithReceiver(factoryRecv, "apply")(
      build = (factory, elems) =>
        factory match
          case f: scala.collection.IterableFactory[scala.collection.Iterable] @unchecked =>
            f.from(elems)
          case _ =>
            try
              val method = factory.getClass.getMethod("from", classOf[scala.collection.IterableOnce[?]])
              method.invoke(factory, elems)
            catch
              case _: Throwable =>
                elems,
      missing = ComptimeError.UnsupportedArity("IterableFactory.apply", "")
    )

private[comptime] object StdlibCollectionHelpers:
  export StdlibCollectionCommon.*
  export StdlibIterableHelpers.*
  export StdlibIteratorHelpers.*
  export StdlibSeqHelpers.*
  export StdlibSetHelpers.*
  export StdlibMapHelpers.*
  export StdlibTuple2Helpers.*
  export StdlibLazyListHelpers.*

import RuleHelpers.*
import StdlibCollectionHelpers.*

// === Rule assembly ===
private[comptime] object StdlibCollectionBaseRules:
  def list(listRecv: RecvPred): List[CallRule] =
    companionRules(listRecv, Nil, xs => xs.toList)

  def vector(vectorRecv: RecvPred): List[CallRule] =
    companionRules(vectorRecv, Vector.empty, xs => xs.toVector)

  def array(arrayRecv: RecvPred): List[CallRule] =
    companionRules(arrayRecv, Array.empty[Any], xs => xs.toArray, emptyAnyArity = true)

  def tuples(tuple2Recv: RecvPred, tuple3Recv: RecvPred, tuple4Recv: RecvPred, tuple5Recv: RecvPred): List[CallRule] =
    tupleRules(tuple2Recv, tuple3Recv, tuple4Recv, tuple5Recv)

private[comptime] object StdlibCollectionSeqRules:
  def rules(
      seqCompanionRecv: RecvPred,
      seqIterRule: RulesFor[Iterable[Any]],
      seqRule: RulesFor[Seq[Any]]
  ): List[CallRule] =
    RuleHelpers.concat(
      companionRules(seqCompanionRecv, Seq.empty[Any], xs => xs.toSeq),
      iterableBasics(seqIterRule),
      iterableFoldLeft(seqIterRule),
      iterableFoldRight(seqIterRule),
      iterableReduce(seqIterRule),
      iterablePartitionMap(seqIterRule),
      iterableGroupMap(seqIterRule),
      iterableGroupMapReduce(seqIterRule),
      iterableTapEach(seqIterRule),
      iterableScanLeft(seqIterRule),
      iterableScanRight(seqIterRule),
      mkStringOps(seqIterRule),
      StdlibCollectionCommon.concatOps(seqIterRule)((xs, other: Iterable[Any]) => xs ++ other),
      seqSliceOps(seqIterRule),
      seqBasics(seqRule),
      iterableMapLike(seqIterRule)
    )

private[comptime] object StdlibCollectionSetRules:
  def rules(setCompanionRecv: RecvPred, setRule: RulesFor[Set[Any]]): List[CallRule] =
    RuleHelpers.concat(
      companionRules(setCompanionRecv, Set.empty[Any], xs => xs.toSet),
      setBasics(setRule)
    )

private[comptime] object StdlibCollectionMapRules:
  def rules(mapCompanionRecv: RecvPred, mapRule: RulesFor[Map[Any, Any]]): List[CallRule] =
    RuleHelpers.concat(
      companionRules(
        mapCompanionRecv,
        Map.empty[Any, Any],
        xs =>
          xs.map {
            case (k, v) => k -> v
            case other  => throw new RuntimeException(s"Map.apply expects tuple args but saw: $other")
          }.toMap
      ),
      mapRule.byName_SLsAny(
        (
          "getOrElse",
          (m, k) => m.get(k),
          (m, k, fallback) => m.getOrElse(k, fallback())
        )
      ),
      mapBasics(mapRule)
    )

private[comptime] object StdlibCollectionIteratorRules:
  def rules(rule: RulesFor[Iterator[Any]]): List[CallRule] =
    RuleHelpers.concat(
      iteratorBasics(rule),
      StdlibIteratorHelpers.iteratorExtras(rule),
      iteratorMapLike(rule),
      iteratorSliceOps(rule)
    )

private[comptime] object StdlibLazyListRules:
  def rules(lazyListCompanionRecv: RecvPred, lazyListRule: RulesFor[LazyList[Any]]): List[CallRule] =
    RuleHelpers.concat(
      companionRules(lazyListCompanionRecv, LazyList.empty[Any], xs => LazyList.from(xs)),
      lazyListBasics(lazyListRule),
      lazyListPredicates(lazyListRule),
      lazyListTransforms(lazyListRule),
      lazyListFolds(lazyListRule),
      lazyListZipOps(lazyListRule),
      lazyListNumericOps(lazyListRule),
      lazyListMkString(lazyListRule),
      lazyListConcat(lazyListRule)
    )

private[comptime] object StdlibCollectionRules:
  private val listRecv           = Recv.module("scala.collection.immutable.List")
  private val vectorRecv         = Recv.module("scala.collection.immutable.Vector")
  private val tuple2Recv         = Recv.moduleOnly("scala.Tuple2")

  // ClassTag.apply rule - handles compiler-inserted ClassTag implicits
  // Uses call.targs to get the type parameter since classOf[T] isn't evaluated at runtime
  private val classTagRecv = Recv.module("scala.reflect.ClassTag")

  // Map type names to ClassTags
  private val classTagsByName: Map[String, ClassTag[Any]] = Map(
    "scala.Int"     -> ClassTag.Int.asInstanceOf[ClassTag[Any]],
    "Int"           -> ClassTag.Int.asInstanceOf[ClassTag[Any]],
    "scala.Long"    -> ClassTag.Long.asInstanceOf[ClassTag[Any]],
    "Long"          -> ClassTag.Long.asInstanceOf[ClassTag[Any]],
    "scala.Double"  -> ClassTag.Double.asInstanceOf[ClassTag[Any]],
    "Double"        -> ClassTag.Double.asInstanceOf[ClassTag[Any]],
    "scala.Float"   -> ClassTag.Float.asInstanceOf[ClassTag[Any]],
    "Float"         -> ClassTag.Float.asInstanceOf[ClassTag[Any]],
    "scala.Boolean" -> ClassTag.Boolean.asInstanceOf[ClassTag[Any]],
    "Boolean"       -> ClassTag.Boolean.asInstanceOf[ClassTag[Any]],
    "scala.Byte"    -> ClassTag.Byte.asInstanceOf[ClassTag[Any]],
    "Byte"          -> ClassTag.Byte.asInstanceOf[ClassTag[Any]],
    "scala.Short"   -> ClassTag.Short.asInstanceOf[ClassTag[Any]],
    "Short"         -> ClassTag.Short.asInstanceOf[ClassTag[Any]],
    "scala.Char"    -> ClassTag.Char.asInstanceOf[ClassTag[Any]],
    "Char"          -> ClassTag.Char.asInstanceOf[ClassTag[Any]],
    "java.lang.String"    -> ClassTag(classOf[String]).asInstanceOf[ClassTag[Any]],
    "scala.Predef.String" -> ClassTag(classOf[String]).asInstanceOf[ClassTag[Any]],
    "String"              -> ClassTag(classOf[String]).asInstanceOf[ClassTag[Any]]
  )

  private val classTagApplyRule: CallRule =
    RuleDsl
      .rule("apply")
      .recv(classTagRecv)
      .anyArity
      .compile("ClassTag.apply") { (call, _) =>
        // Extract type from targs - ClassTag.apply[T] has T as the type argument
        val classTag = call.targs.headOption match
          case Some(TypeIR.Ref(typeName, _)) =>
            classTagsByName.getOrElse(typeName, ClassTag.Any.asInstanceOf[ClassTag[Any]])
          case _ =>
            ClassTag.Any.asInstanceOf[ClassTag[Any]]
        Right(Eval.Value(classTag))
      }
  private val tuple3Recv         = Recv.moduleOnly("scala.Tuple3")
  private val tuple4Recv         = Recv.moduleOnly("scala.Tuple4")
  private val tuple5Recv         = Recv.moduleOnly("scala.Tuple5")
  private val arrayCompanionRecv = Recv.moduleOnly("scala.Array")
  private val arrayInstanceRecv  = Recv.union("scala.Array") // For arr.apply(i), arr.length, etc.
  private val seqCompanionRecv   = Recv.modules("scala.collection.Seq", "scala.collection.immutable.Seq")
  private val setCompanionRecv   = Recv.modules("scala.collection.Set", "scala.collection.immutable.Set")
  private val mapCompanionRecv =
    Recv.modules("scala.collection.Map", "scala.collection.immutable.Map", "scala.collection.MapFactory")
  private val iterableFactoryRecv = Recv.union("scala.collection.IterableFactory")
  private val seqRecv =
    Recv.union(
      "scala.collection.immutable.List",
      "scala.collection.immutable.$colon$colon",
      "scala.collection.immutable.Nil$",
      "scala.collection.immutable.Vector",
      "scala.collection.immutable.Seq",
      "scala.collection.Seq",
      "scala.collection.IndexedSeq", // Vector.sameElements
      "scala.collection.immutable.IndexedSeq",
      "scala.collection.IterableOps",
      "scala.collection.IterableOnceOps", // Vector methods like forall, find, count report this as owner
      "scala.collection.StrictOptimizedIterableOps",
      "scala.collection.StrictOptimizedLinearSeqOps",
      "scala.collection.StrictOptimizedSeqOps",           // sorted, padTo
      "scala.collection.immutable.StrictOptimizedSeqOps", // distinctBy
      "scala.collection.LinearSeqOps",
      "scala.collection.IndexedSeqOps",
      "scala.collection.SeqOps",  // sortBy, sortWith, etc.
      "scala.collection.ArrayOps" // Array operations via wrapXxxArray -> ArraySeq
    )
  private val setRecv = Recv.union(
    "scala.collection.immutable.Set",
    "scala.collection.Set",
    "scala.collection.SetOps",
    "scala.collection.immutable.SetOps",
    "scala.collection.StrictOptimizedSetOps",
    "scala.collection.immutable.StrictOptimizedSetOps"
  )
  private val mapRecv = Recv.union(
    "scala.collection.immutable.Map",
    "scala.collection.Map",
    "scala.collection.MapOps",
    "scala.collection.immutable.MapOps"
  )
  private val iteratorRecv          = Recv.module("scala.collection.Iterator")
  private val lazyListCompanionRecv = Recv.module("scala.collection.immutable.LazyList")
  private val lazyListRecv = Recv.union(
    "scala.collection.immutable.LazyList",
    "scala.collection.immutable.LazyListIterable",
    "scala.collection.IterableOps",
    "scala.collection.IterableOnceOps",
    "scala.collection.SeqOps",
    "scala.collection.LinearSeqOps"
  )
  private val seqIterRule  = RulesFor.iterable(seqRecv)
  private val seqRule      = RulesFor.seq(seqRecv)
  private val setRule      = RulesFor.set(setRecv)
  private val mapRule      = RulesFor.map(mapRecv)
  private val iteratorRule = RulesFor.iterator(iteratorRecv)
  private val lazyListRule = RulesFor[LazyList[Any]](lazyListRecv)

  // Tuple2 instance receiver (for swap, etc.) - includes the actual Tuple2 instances
  private val tuple2InstanceRecv = Recv.union("scala.Tuple2")
  private val tuple2InstanceRule = RulesFor[(Any, Any)](tuple2InstanceRecv)

  // Factory methods (fill, tabulate) - dispatched through various factory traits
  private val seqFactoryRecv = Recv.union(
    "scala.collection.StrictOptimizedSeqFactory",
    "scala.collection.IterableFactory" // For Seq.fill, etc.
  )
  private val seqFactoryRule = RulesFor.any(seqFactoryRecv)

  // fill(n)(elem) - curried with by-name second arg (flattened to A2 in CallIR)
  // Uses actual factory to preserve collection type (Vector.fill returns Vector, etc.)
  private val fillRule: CallRule =
    seqFactoryRule.byName_SL[Int, Any, Any]("fill")(
      // Fast path for n=0 - use factory's empty to preserve type
      (factory, n) =>
        if n == 0 then
          factory match
            case f: scala.collection.IterableFactory[?] =>
              Some(f.empty)
            case _ => None
        else None
    ) { (factory, n, elemThunk) =>
      factory match
        case f: scala.collection.IterableFactory[?] =>
          f.fill(n)(elemThunk())
        case _ =>
          // Reflection fallback for non-standard factories
          val method = factory.getClass.getMethod("fill", classOf[Int], classOf[scala.Function0[?]])
          method.invoke(factory, n.asInstanceOf[java.lang.Integer], (() => elemThunk()): scala.Function0[Any])
    }

  // tabulate(n)(f) - curried with function second arg (flattened to A2 in CallIR)
  // Uses actual factory to preserve collection type (Vector.tabulate returns Vector, etc.)
  private val tabulateRule: CallRule =
    seqFactoryRule.rule2[Int, Int => Any, Any]("tabulate") { (factory, n, f) =>
      factory match
        case f2: scala.collection.IterableFactory[?] =>
          f2.tabulate(n)(f)
        case _ =>
          // Reflection fallback for non-standard factories
          val method = factory.getClass.getMethod("tabulate", classOf[Int], classOf[scala.Function1[?, ?]])
          method.invoke(factory, n.asInstanceOf[java.lang.Integer], f)
    }

  private val seqFactoryRules: List[CallRule] =
    List(fillRule, tabulateRule)

  private val listRules: List[CallRule] =
    StdlibCollectionBaseRules.list(listRecv)

  private val vectorRules: List[CallRule] =
    StdlibCollectionBaseRules.vector(vectorRecv)

  private val arrayCompanionRules: List[CallRule] =
    StdlibCollectionBaseRules.array(arrayCompanionRecv)

  // Array.fill and Array.tabulate - special handling since Array requires ClassTag (implicit)
  // Array.fill has arity=(3): (n, elem, classTag) - we use byName_SLL to handle the 3-arg form
  // We always use ClassTag.Any to create Object[] arrays to avoid primitive array casting issues
  // (primitive int[] can't be cast to Object[] which causes ClassCastException)
  private val arrayFactoryRule = RulesFor.any(arrayCompanionRecv)

  private val arrayFillRule: CallRule =
    arrayFactoryRule.byName_SLL[Int, Any, Any, Array[Any]]("fill")((_, n) =>
      if n == 0 then Some(Array.empty[Any]) else None
    ) { (_, n, elemThunk, _) =>
      val elem = elemThunk()
      // Always use ClassTag.Any to create Object[] - avoids primitive array casting issues
      Array.fill(n)(elem)(ClassTag.Any)
    }

  private val arrayTabulateRule: CallRule =
    arrayFactoryRule.rule3[Int, Int => Any, Any, Array[Any]]("tabulate") { (_, n, f, _) =>
      if n == 0 then Array.empty[Any]
      else
        // Always use ClassTag.Any to create Object[] - avoids primitive array casting issues
        Array.tabulate(n)(f)(ClassTag.Any)
    }

  private val arrayFactoryRules: List[CallRule] =
    List(arrayFillRule, arrayTabulateRule)

  // Array instance rules (arr.apply(i), arr.length, etc.)
  private val arrayInstanceRules: List[CallRule] =
    import RuleHelpers.*
    List(
      ruleRecv1[Array[Any], Int, Any](arrayInstanceRecv, "apply")((arr, idx) => arr(idx)),
      ruleRecv0[Array[Any], Int](arrayInstanceRecv, "length")(_.length),
      ruleRecv0[Array[Any], Int](arrayInstanceRecv, "size")(_.length),
      ruleRecv0[Array[Any], Boolean](arrayInstanceRecv, "isEmpty")(_.isEmpty),
      ruleRecv0[Array[Any], Boolean](arrayInstanceRecv, "nonEmpty")(_.nonEmpty)
    )

  private val seqRules: List[CallRule] =
    StdlibCollectionSeqRules.rules(seqCompanionRecv, seqIterRule, seqRule)

  private val setRules: List[CallRule] =
    StdlibCollectionSetRules.rules(setCompanionRecv, setRule)

  private val mapRules: List[CallRule] =
    StdlibCollectionMapRules.rules(mapCompanionRecv, mapRule)

  private val tupleRuleList: List[CallRule] =
    StdlibCollectionBaseRules.tuples(tuple2Recv, tuple3Recv, tuple4Recv, tuple5Recv)

  private val iteratorRules: List[CallRule] =
    StdlibCollectionIteratorRules.rules(iteratorRule)

  private val lazyListRules: List[CallRule] =
    StdlibLazyListRules.rules(lazyListCompanionRecv, lazyListRule)

  // Iterator.from and LazyList.from companion methods
  private val iteratorCompanionRule = RulesFor.any(iteratorRecv)
  private val lazyListCompanionRule = RulesFor.any(lazyListCompanionRecv)

  private val iteratorFromRules: List[CallRule] =
    RuleHelpers.concat(
      // Iterator.from(start)
      iteratorCompanionRule.rules[Int, Iterator[Int]]("from")((_, start) => Iterator.from(start)),
      // Iterator.from(start, step)
      iteratorCompanionRule.rules[Int, Int, Iterator[Int]]("from")((_, start, step) => Iterator.from(start, step))
    )

  private val lazyListFromRules: List[CallRule] =
    RuleHelpers.concat(
      // LazyList.from(start)
      lazyListCompanionRule.rules[Int, LazyList[Int]]("from")((_, start) => LazyList.from(start)),
      // LazyList.from(start, step)
      lazyListCompanionRule.rules[Int, Int, LazyList[Int]]("from")((_, start, step) => LazyList.from(start, step))
    )

  private val iterableFactoryRules: List[CallRule] =
    StdlibCollectionCompanionRules.iterableFactoryApply(iterableFactoryRecv) :: Nil

  private val tuple2InstanceRules: List[CallRule] =
    tuple2Swap(tuple2InstanceRule)

  // WithFilter support for for-comprehensions
  // Our withFilter implementation returns a filtered collection/option, so when
  // WithFilter.map/flatMap/etc is called, the receiver is the filtered result.
  // We delegate to the standard operations.

  // Collection WithFilter
  private val collectionWithFilterRecv = Recv.union(
    "scala.collection.WithFilter",
    "scala.collection.IterableOps.WithFilter",
    "scala.collection.IterableOps$WithFilter"
  )
  private val collectionWithFilterRule = RulesFor.iterable(collectionWithFilterRecv)
  private val collectionWithFilterRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibCollectionCommon.mapFlatMapOps[Iterable[Any], Iterable[Any]](collectionWithFilterRule)(
        (xs, f) => xs.map(f),
        (xs, f) => xs.flatMap(f)
      ),
      collectionWithFilterRule.ops1PredList(
        List(
          "withFilter" -> ((xs: Iterable[Any], f: Any => Boolean) => xs.filter(f))
        )
      )
    )

  // Option WithFilter - receiver is Option[Any] since our withFilter returns filtered Option
  private val optionWithFilterRecv = Recv.union(
    "scala.Option.WithFilter",
    "scala.Option$WithFilter"
  )
  private val optionWithFilterRule = RulesFor.option(optionWithFilterRecv)
  private val optionWithFilterRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibCollectionCommon.mapFlatMapOps[Option[Any], Option[Any]](optionWithFilterRule)(
        (opt, f) => opt.map(f),
        (opt, f) => opt.flatMap(f)
      ),
      optionWithFilterRule.ops1PredList(
        List(
          "withFilter" -> ((opt: Option[Any], f: Any => Boolean) => opt.filter(f))
        )
      )
    )

  private val withFilterRules: List[CallRule] =
    RuleHelpers.concat(collectionWithFilterRules, optionWithFilterRules)

  val rules: List[CallRule] =
    RuleHelpers.concat(
      List(classTagApplyRule),
      listRules,
      vectorRules,
      iterableFactoryRules,
      seqFactoryRules,
      seqRules,
      setRules,
      mapRules,
      arrayCompanionRules,
      arrayFactoryRules,
      arrayInstanceRules,
      tupleRuleList,
      tuple2InstanceRules,
      iteratorRules,
      iteratorFromRules,
      lazyListRules,
      lazyListFromRules,
      withFilterRules
    )
