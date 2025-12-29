// Stdlib rules (hand-maintained).
package comptime

import RuleHelpersCore.*

private[comptime] object StdlibPredefTables:
  val stringIds: List[(String, String => Any)] = List(
    "augmentString" -> identity,
    "wrapString"    -> identity
  )

  val identityOps: List[(String, Any => Any)] = List(
    "identity" -> identity
  )

  val intWrappers: List[(String, Int => Any)] = List(
    "intWrapper" -> (i => new scala.runtime.RichInt(i))
  )

  val charIds: List[(String, Char => Any)] = List(
    "charWrapper" -> identity
  )

  // byteWrapper/shortWrapper receive Int literals from type ascription like (5: Byte)
  // The AST loses the Typed node after inlining, so we convert here
  val byteIds: List[(String, Any => Any)] = List(
    "byteWrapper" -> ((x: Any) =>
      x match
        case b: Byte   => b
        case n: Number => n.byteValue()
    )
  )

  val shortIds: List[(String, Any => Any)] = List(
    "shortWrapper" -> ((x: Any) =>
      x match
        case s: Short  => s
        case n: Number => n.shortValue()
    )
  )

  val floatIds: List[(String, Float => Any)] = List(
    "floatWrapper" -> (f => new scala.runtime.RichFloat(f))
  )

  val longWrappers: List[(String, Long => Any)] = List(
    "longWrapper" -> (l => new scala.runtime.RichLong(l))
  )

  val doubleWrappers: List[(String, Double => Any)] = List(
    "doubleWrapper" -> (d => new scala.runtime.RichDouble(d))
  )

  val arrayWraps: List[(String, Array[AnyRef] => Any)] = List(
    "wrapRefArray" -> (arr => arr.toSeq)
  )

  private def arrayToIndexedSeq(value: Any, name: String): Any =
    value match
      case arr: Array[Int]              => arr.toIndexedSeq
      case arr: Array[Long]             => arr.toIndexedSeq
      case arr: Array[Double]           => arr.toIndexedSeq
      case arr: Array[Float]            => arr.toIndexedSeq
      case arr: Array[Boolean]          => arr.toIndexedSeq
      case arr: Array[Byte]             => arr.toIndexedSeq
      case arr: Array[Short]            => arr.toIndexedSeq
      case arr: Array[Char]             => arr.toIndexedSeq
      case arr: Array[AnyRef]           => arr.toIndexedSeq
      case seq: scala.collection.Seq[?] => seq // Already a Seq, pass through
      case other                        => throw new RuntimeException(s"$name expects Array but got: ${other.getClass}")

  // Array wrappers - convert arrays to IndexedSeq for Seq operations
  // Accept Any to handle boxed arrays produced by comptime (and primitive arrays if present).
  val intArrayWraps: List[(String, Any => Any)] = List(
    "wrapIntArray" -> (arr => arrayToIndexedSeq(arr, "wrapIntArray")),
    "intArrayOps"  -> (arr => arrayToIndexedSeq(arr, "intArrayOps"))
  )

  val longArrayWraps: List[(String, Any => Any)] = List(
    "wrapLongArray" -> (arr => arrayToIndexedSeq(arr, "wrapLongArray")),
    "longArrayOps"  -> (arr => arrayToIndexedSeq(arr, "longArrayOps"))
  )

  val doubleArrayWraps: List[(String, Any => Any)] = List(
    "wrapDoubleArray" -> (arr => arrayToIndexedSeq(arr, "wrapDoubleArray")),
    "doubleArrayOps"  -> (arr => arrayToIndexedSeq(arr, "doubleArrayOps"))
  )

  val floatArrayWraps: List[(String, Any => Any)] = List(
    "wrapFloatArray" -> (arr => arrayToIndexedSeq(arr, "wrapFloatArray")),
    "floatArrayOps"  -> (arr => arrayToIndexedSeq(arr, "floatArrayOps"))
  )

  val booleanArrayWraps: List[(String, Any => Any)] = List(
    "wrapBooleanArray" -> (arr => arrayToIndexedSeq(arr, "wrapBooleanArray")),
    "booleanArrayOps"  -> (arr => arrayToIndexedSeq(arr, "booleanArrayOps"))
  )

  val byteArrayWraps: List[(String, Any => Any)] = List(
    "wrapByteArray" -> (arr => arrayToIndexedSeq(arr, "wrapByteArray")),
    "byteArrayOps"  -> (arr => arrayToIndexedSeq(arr, "byteArrayOps"))
  )

  val shortArrayWraps: List[(String, Any => Any)] = List(
    "wrapShortArray" -> (arr => arrayToIndexedSeq(arr, "wrapShortArray")),
    "shortArrayOps"  -> (arr => arrayToIndexedSeq(arr, "shortArrayOps"))
  )

  val charArrayWraps: List[(String, Any => Any)] = List(
    "wrapCharArray" -> (arr => arrayToIndexedSeq(arr, "wrapCharArray")),
    "charArrayOps"  -> (arr => arrayToIndexedSeq(arr, "charArrayOps"))
  )

  // Reference array ops
  val refArrayOps: List[(String, Array[AnyRef] => Any)] = List(
    "refArrayOps" -> (arr => arr.toIndexedSeq)
  )

  val arrowAssoc: List[(String, Any => Any)] = List(
    "ArrowAssoc" -> (a => new scala.Predef.ArrowAssoc(a))
  )

  // Numeric widening conversions (implicit conversions in Predef)
  val int2long: List[(String, Int => Any)] = List(
    "int2long"   -> (_.toLong),
    "int2float"  -> (_.toFloat),
    "int2double" -> (_.toDouble)
  )

  val long2float: List[(String, Long => Any)] = List(
    "long2float"  -> (_.toFloat),
    "long2double" -> (_.toDouble)
  )

  val float2double: List[(String, Float => Any)] = List(
    "float2double" -> (_.toDouble)
  )

  val byte2short: List[(String, Byte => Any)] = List(
    "byte2short"  -> (_.toShort),
    "byte2int"    -> (_.toInt),
    "byte2long"   -> (_.toLong),
    "byte2float"  -> (_.toFloat),
    "byte2double" -> (_.toDouble)
  )

  val short2int: List[(String, Short => Any)] = List(
    "short2int"    -> (_.toInt),
    "short2long"   -> (_.toLong),
    "short2float"  -> (_.toFloat),
    "short2double" -> (_.toDouble)
  )

  val char2int: List[(String, Char => Any)] = List(
    "char2int"    -> (_.toInt),
    "char2long"   -> (_.toLong),
    "char2float"  -> (_.toFloat),
    "char2double" -> (_.toDouble)
  )

  val option2Iterable: List[(String, Option[Any] => Any)] = List(
    "option2Iterable" -> (_.toList)
  )

  val arrowAssocOps: List[(String, (scala.Predef.ArrowAssoc[Any], Any) => Any)] = List(
    "->" -> ((a, b) => a.->(b))
  )

  val eqOps: List[(String, (Any, Any) => Any)] = List(
    "==" -> (_ == _),
    "!=" -> (_ != _)
  )

  val castOps: List[(String, Any => Any)] = List(
    "$asInstanceOf$" -> identity,
    "asInstanceOf"   -> identity
  )

private[comptime] object StdlibPredefRules:
  private val predefRecv     = Recv.modules("scala.Predef", "scala.LowPriorityImplicits")
  private val optionConvRecv = Recv.modules("scala.Option", "scala.Predef")
  private val anyRule        = RulesFor.any(AnyRecv)

  private val predefRules: List[CallRule] =
    RuleHelpers.concat(
      ruleStatic1sList[String, Any](predefRecv, StdlibPredefTables.stringIds),
      ruleStatic1sList[Int, Any](predefRecv, StdlibPredefTables.intWrappers),
      ruleStatic1sList[Long, Any](predefRecv, StdlibPredefTables.longWrappers),
      ruleStatic1sList[Double, Any](predefRecv, StdlibPredefTables.doubleWrappers),
      ruleStatic1sList[Char, Any](predefRecv, StdlibPredefTables.charIds),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.byteIds),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.shortIds),
      ruleStatic1sList[Float, Any](predefRecv, StdlibPredefTables.floatIds),
      ruleStatic1sList[Array[AnyRef], Any](predefRecv, StdlibPredefTables.arrayWraps),
      ruleStatic1sList[Array[AnyRef], Any](predefRecv, StdlibPredefTables.refArrayOps),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.intArrayWraps),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.longArrayWraps),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.doubleArrayWraps),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.floatArrayWraps),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.booleanArrayWraps),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.byteArrayWraps),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.shortArrayWraps),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.charArrayWraps),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.arrowAssoc),
      ruleStatic1sList[Any, Any](predefRecv, StdlibPredefTables.identityOps),
      ruleStatic1sList[Option[Any], Any](optionConvRecv, StdlibPredefTables.option2Iterable),
      // Numeric widening conversions
      ruleStatic1sList[Int, Any](predefRecv, StdlibPredefTables.int2long),
      ruleStatic1sList[Long, Any](predefRecv, StdlibPredefTables.long2float),
      ruleStatic1sList[Float, Any](predefRecv, StdlibPredefTables.float2double),
      ruleStatic1sList[Byte, Any](predefRecv, StdlibPredefTables.byte2short),
      ruleStatic1sList[Short, Any](predefRecv, StdlibPredefTables.short2int),
      ruleStatic1sList[Char, Any](predefRecv, StdlibPredefTables.char2int),
      anyRule.consts(
        "$conforms" -> scala.Predef.$conforms[Any],
        "refl"      -> ((x: Any) => x)
      )
    )

  private val arrowAssocRecv = Recv.union("scala.Predef$.ArrowAssoc", "scala.Predef$ArrowAssoc")
  private val arrowAssocRules: List[CallRule] =
    RulesFor[scala.Predef.ArrowAssoc[Any]](arrowAssocRecv).ops1AnyList(StdlibPredefTables.arrowAssocOps)

  private val anyEqRules: List[CallRule] =
    anyRule.ops1AnyList(StdlibPredefTables.eqOps)

  private val anyCastRules: List[CallRule] =
    anyRule.opsList(StdlibPredefTables.castOps)

  val rules: List[CallRule] =
    RuleHelpers.concat(
      predefRules,
      arrowAssocRules,
      anyCastRules,
      anyEqRules
    )
