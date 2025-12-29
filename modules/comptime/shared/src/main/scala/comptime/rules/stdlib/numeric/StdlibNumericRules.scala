// Stdlib rules (hand-maintained).
package comptime

private[comptime] object StdlibNumericTables:
  val boolUnary: List[(String, Boolean => Any)] = List(
    "unary_!" -> (!_)
  )

  val boolBitwise: List[(String, (Boolean, Boolean) => Any)] = List(
    "&" -> (_ & _),
    "|" -> (_ | _),
    "^" -> (_ ^ _)
  )

  val boolByName: List[(String, (Boolean, () => Boolean) => Any)] = List(
    "&&" -> ((a, b) => if a then b() else false),
    "||" -> ((a, b) => if a then true else b())
  )

  val charBasics: List[(String, Char => Any)] = List(
    "isDigit"  -> (_.isDigit),
    "isLetter" -> (_.isLetter),
    "isUpper"  -> (_.isUpper),
    "isLower"  -> (_.isLower),
    "toInt"    -> (_.toInt)
  )

  // RichChar methods (toUpper, toLower, asDigit)
  val richCharMethods: List[(String, Char => Any)] = List(
    "toUpper"         -> (_.toUpper),
    "toLower"         -> (_.toLower),
    "asDigit"         -> (_.asDigit),
    "isDigit"         -> (_.isDigit),
    "isLetter"        -> (_.isLetter),
    "isWhitespace"    -> (_.isWhitespace),
    "isUpper"         -> (_.isUpper),
    "isLower"         -> (_.isLower),
    "isLetterOrDigit" -> (_.isLetterOrDigit),
    "isControl"       -> (_.isControl),
    "isSpaceChar"     -> (_.isSpaceChar)
  )

  // Char arithmetic with Int (returns Int)
  val charIntArith: List[(String, (Char, Int) => Any)] = List(
    "+" -> ((c: Char, i: Int) => c + i),
    "-" -> ((c: Char, i: Int) => c - i)
  )

  // Char minus Char (returns Int)
  val charCharArith: List[(String, (Char, Char) => Any)] = List(
    "-" -> ((a: Char, b: Char) => a - b)
  )

  val charRange: List[(String, (Char, Char) => Any)] = List(
    "to"    -> ((a, b) => a.to(b)),
    "until" -> ((a, b) => a.until(b))
  )

  val intRange: List[(String, (Int, Int) => Any)] = List(
    "to"    -> ((a, b) => a.to(b)),
    "until" -> ((a, b) => a.until(b))
  )

  // Int bitwise operations
  val intBitwise: List[(String, (Int, Int) => Any)] = List(
    "<<"  -> (_ << _),
    ">>"  -> (_ >> _),
    ">>>" -> (_ >>> _),
    "&"   -> (_ & _),
    "|"   -> (_ | _),
    "^"   -> (_ ^ _)
  )

  // Long bitwise operations
  val longBitwise: List[(String, (Long, Int) => Any)] = List(
    "<<"  -> (_ << _),
    ">>"  -> (_ >> _),
    ">>>" -> (_ >>> _)
  )

  val longBitwiseLong: List[(String, (Long, Long) => Any)] = List(
    "&" -> (_ & _),
    "|" -> (_ | _),
    "^" -> (_ ^ _)
  )

  // Int conversions
  val intConversions: List[(String, Int => Any)] = List(
    "toLong"         -> (_.toLong),
    "toDouble"       -> (_.toDouble),
    "toFloat"        -> (_.toFloat),
    "toShort"        -> (_.toShort),
    "toByte"         -> (_.toByte),
    "toChar"         -> (_.toChar),
    "toHexString"    -> (_.toHexString),
    "toBinaryString" -> (_.toBinaryString)
  )

  // Long conversions
  val longConversions: List[(String, Long => Any)] = List(
    "toInt"    -> (_.toInt),
    "toDouble" -> (_.toDouble),
    "toFloat"  -> (_.toFloat)
  )

  // RichInt methods - receiver is the RichInt wrapper
  val richIntRange: List[(String, (scala.runtime.RichInt, Int) => Any)] = List(
    "to"    -> ((rich, b) => rich.to(b)),
    "until" -> ((rich, b) => rich.until(b))
  )

  // RichInt max/min - these take an Int argument
  val richIntMinMax: List[(String, (scala.runtime.RichInt, Int) => Any)] = List(
    "max" -> ((rich, b) => rich.max(b)),
    "min" -> ((rich, b) => rich.min(b))
  )

  // RichLong max/min
  val richLongMinMax: List[(String, (scala.runtime.RichLong, Long) => Any)] = List(
    "max" -> ((rich, b) => rich.max(b)),
    "min" -> ((rich, b) => rich.min(b))
  )

  // RichDouble max/min
  val richDoubleMinMax: List[(String, (scala.runtime.RichDouble, Double) => Any)] = List(
    "max" -> ((rich, b) => rich.max(b)),
    "min" -> ((rich, b) => rich.min(b))
  )

  // RichFloat max/min
  val richFloatMinMax: List[(String, (scala.runtime.RichFloat, Float) => Any)] = List(
    "max" -> ((rich, b) => rich.max(b)),
    "min" -> ((rich, b) => rich.min(b))
  )

  // Helper to convert Any to numeric types (handles Int literals passed as Byte/Short args)
  private def toByte(x: Any): Byte = x match
    case b: Byte   => b
    case n: Number => n.byteValue()
  private def toShort(x: Any): Short = x match
    case s: Short  => s
    case n: Number => n.shortValue()

  // Byte primitive unary operations (extension methods on scala.Byte)
  // Accept Any because type ascription like ((5): Byte) still passes Int at runtime
  // Note: .sign returns Byte (not Int), .signum returns Int
  val byteUnary: List[(String, Any => Any)] = List(
    "abs"  -> (x => Math.abs(toByte(x).toInt).toByte),
    "sign" -> (x => Integer.signum(toByte(x).toInt).toByte)
  )

  // RichByte unary operations (via scala.runtime.RichByte)
  val richByteUnary: List[(String, Any => Any)] = List(
    "abs"    -> (x => Math.abs(toByte(x).toInt).toByte),
    "sign"   -> (x => Integer.signum(toByte(x).toInt).toByte),
    "signum" -> (x => Integer.signum(toByte(x).toInt))
  )

  // Short primitive unary operations (extension methods on scala.Short)
  // Accept Any because type ascription like ((5): Short) still passes Int at runtime
  // Note: .sign returns Short (not Int), .signum returns Int
  val shortUnary: List[(String, Any => Any)] = List(
    "abs"  -> (x => Math.abs(toShort(x).toInt).toShort),
    "sign" -> (x => Integer.signum(toShort(x).toInt).toShort)
  )

  // RichShort unary operations (via scala.runtime.RichShort)
  val richShortUnary: List[(String, Any => Any)] = List(
    "abs"    -> (x => Math.abs(toShort(x).toInt).toShort),
    "sign"   -> (x => Integer.signum(toShort(x).toInt).toShort),
    "signum" -> (x => Integer.signum(toShort(x).toInt))
  )

  // BigInt operations
  val bigIntUnary: List[(String, BigInt => Any)] = List(
    "unary_-"   -> (-_),
    "abs"       -> (_.abs),
    "signum"    -> (_.signum),
    "bitLength" -> (_.bitLength)
  )

  val bigIntArith: List[(String, (BigInt, BigInt) => Any)] = List(
    "+"   -> (_ + _),
    "-"   -> (_ - _),
    "*"   -> (_ * _),
    "/"   -> (_ / _),
    "%"   -> (_ % _),
    "mod" -> (_.mod(_)),
    "min" -> (_.min(_)),
    "max" -> (_.max(_))
  )

  val bigIntCompare: List[(String, (BigInt, BigInt) => Any)] = List(
    "=="      -> (_ == _),
    "!="      -> (_ != _),
    "<"       -> (_ < _),
    "<="      -> (_ <= _),
    ">"       -> (_ > _),
    ">="      -> (_ >= _),
    "compare" -> (_.compare(_))
  )

  val bigIntConversions: List[(String, BigInt => Any)] = List(
    "toInt"    -> (_.toInt),
    "toLong"   -> (_.toLong),
    "toDouble" -> (_.toDouble),
    "toString" -> (_.toString)
  )

  val bigIntPow: List[(String, (BigInt, Int) => Any)] = List(
    "pow" -> (_.pow(_))
  )

  // BigDecimal operations
  val bigDecimalUnary: List[(String, BigDecimal => Any)] = List(
    "unary_-"   -> (-_),
    "abs"       -> (_.abs),
    "signum"    -> (_.signum),
    "precision" -> (_.precision),
    "scale"     -> (_.scale)
  )

  val bigDecimalArith: List[(String, (BigDecimal, BigDecimal) => Any)] = List(
    "+"   -> (_ + _),
    "-"   -> (_ - _),
    "*"   -> (_ * _),
    "/"   -> (_ / _),
    "%"   -> (_ % _),
    "min" -> (_.min(_)),
    "max" -> (_.max(_))
  )

  val bigDecimalCompare: List[(String, (BigDecimal, BigDecimal) => Any)] = List(
    "=="      -> (_ == _),
    "!="      -> (_ != _),
    "<"       -> (_ < _),
    "<="      -> (_ <= _),
    ">"       -> (_ > _),
    ">="      -> (_ >= _),
    "compare" -> (_.compare(_))
  )

  val bigDecimalConversions: List[(String, BigDecimal => Any)] = List(
    "toInt"    -> (_.toInt),
    "toLong"   -> (_.toLong),
    "toDouble" -> (_.toDouble),
    "toString" -> (_.toString)
  )
  // Note: setScale with RoundingMode is not yet supported because
  // Scala 2-style enumerations need special handling at compile time

private[comptime] object StdlibNumericHelpers:
  import RuleHelpers.*

  def arithOps[A: scala.reflect.ClassTag](rule: RulesFor[A])(
      unary: A => A,
      add: (A, A) => A,
      sub: (A, A) => A,
      mul: (A, A) => A,
      div: (A, A) => A,
      mod: (A, A) => A
  ): List[CallRule] =
    RuleHelpers.concat(
      rule.ops(
        "unary_-" -> unary
      ),
      rule.ops1[A](
        "+" -> add,
        "-" -> sub,
        "*" -> mul,
        "/" -> div,
        "%" -> mod
      )
    )

  def comparisons[A: scala.reflect.ClassTag](rule: RulesFor[A])(using ord: Ordering[A]): List[CallRule] =
    rule.ops1[A](
      "==" -> (ord.equiv),
      "!=" -> ((a: A, b: A) => !ord.equiv(a, b)),
      "<"  -> (ord.lt),
      "<=" -> (ord.lteq),
      ">"  -> (ord.gt),
      ">=" -> (ord.gteq)
    )

  def toLong(value: Any): Long =
    value match
      case v: Long   => v
      case v: Int    => v.toLong
      case v: Short  => v.toLong
      case v: Byte   => v.toLong
      case v: Char   => v.toLong
      case v: Float  => v.toLong
      case v: Double => v.toLong
      case other     => throw new RuntimeException(s"Unsupported numeric value: $other")

  def toInt(value: Any): Int =
    value match
      case v: Int    => v
      case v: Short  => v.toInt
      case v: Byte   => v.toInt
      case v: Char   => v.toInt
      case v: Long   => v.toInt
      case v: Float  => v.toInt
      case v: Double => v.toInt
      case other     => throw new RuntimeException(s"Unsupported numeric value: $other")

  def toDouble(value: Any): Double =
    value match
      case v: Double => v
      case v: Float  => v.toDouble
      case v: Long   => v.toDouble
      case v: Int    => v.toDouble
      case v: Short  => v.toDouble
      case v: Byte   => v.toDouble
      case v: Char   => v.toDouble
      case other     => throw new RuntimeException(s"Unsupported numeric value: $other")

  def toFloat(value: Any): Float =
    value match
      case v: Float  => v
      case v: Double => v.toFloat
      case v: Long   => v.toFloat
      case v: Int    => v.toFloat
      case v: Short  => v.toFloat
      case v: Byte   => v.toFloat
      case v: Char   => v.toFloat
      case other     => throw new RuntimeException(s"Unsupported numeric value: $other")

  // Scala numeric widening: Double > Float > Long > Int (Byte/Short/Char promote to Int)
  // Returns: 0=Int, 1=Long, 2=Float, 3=Double
  private def numericRank(value: Any): Int =
    value match
      case _: Double => 3
      case _: Float  => 2
      case _: Long   => 1
      case _         => 0 // Int, Short, Byte, Char → Int

  // Promotes both values to the widest type and applies the operation
  private inline def withPromotion[R](a: Any, b: Any)(
      intOp: (Int, Int) => R,
      longOp: (Long, Long) => R,
      floatOp: (Float, Float) => R,
      doubleOp: (Double, Double) => R
  ): R =
    val rank = numericRank(a) max numericRank(b)
    rank match
      case 3 => doubleOp(toDouble(a), toDouble(b))
      case 2 => floatOp(toFloat(a), toFloat(b))
      case 1 => longOp(toLong(a), toLong(b))
      case _ => intOp(toInt(a), toInt(b))

  // Comparison with proper bidirectional promotion (widest type wins)
  def comparisonsPromoted(rule: RulesFor[?]): List[CallRule] =
    RulesFor[Any](rule.recv).ops1Any(
      "==" -> ((a: Any, b: Any) => withPromotion(a, b)(_ == _, _ == _, _ == _, _ == _)),
      "!=" -> ((a: Any, b: Any) => withPromotion(a, b)(_ != _, _ != _, _ != _, _ != _)),
      "<"  -> ((a: Any, b: Any) => withPromotion(a, b)(_ < _, _ < _, _ < _, _ < _)),
      "<=" -> ((a: Any, b: Any) => withPromotion(a, b)(_ <= _, _ <= _, _ <= _, _ <= _)),
      ">"  -> ((a: Any, b: Any) => withPromotion(a, b)(_ > _, _ > _, _ > _, _ > _)),
      ">=" -> ((a: Any, b: Any) => withPromotion(a, b)(_ >= _, _ >= _, _ >= _, _ >= _))
    )

  // Arithmetic with proper bidirectional promotion (widest type wins)
  def arithOpsPromoted(rule: RulesFor[?]): List[CallRule] =
    RulesFor[Any](rule.recv).ops1Any(
      "+" -> ((a: Any, b: Any) => withPromotion(a, b)(_ + _, _ + _, _ + _, _ + _)),
      "-" -> ((a: Any, b: Any) => withPromotion(a, b)(_ - _, _ - _, _ - _, _ - _)),
      "*" -> ((a: Any, b: Any) => withPromotion(a, b)(_ * _, _ * _, _ * _, _ * _)),
      "/" -> ((a: Any, b: Any) => withPromotion(a, b)(_ / _, _ / _, _ / _, _ / _)),
      "%" -> ((a: Any, b: Any) => withPromotion(a, b)(_ % _, _ % _, _ % _, _ % _))
    )

  // DEPRECATED: These old functions had bugs (narrowing instead of widening).
  // Use comparisonsPromoted and arithOpsPromoted instead.
  // Keeping temporarily for reference, can be removed in future cleanup.

private[comptime] object StdlibNumericBasicRules:
  private def charBasics(rule: RulesFor[Char]): List[CallRule] =
    RuleHelpers.concat(
      // Use comparisonsPromoted for Char so it handles type widening correctly
      // (e.g., c >= '0' where c: Char might be typed as scala.Int.>=)
      StdlibNumericHelpers.comparisonsPromoted(rule),
      rule.opsList(StdlibNumericTables.charBasics)
    )

  def bool(rule: RulesFor[Boolean]): List[CallRule] =
    RuleHelpers.concat(
      rule.opsList(StdlibNumericTables.boolUnary),
      StdlibNumericHelpers.comparisons(rule),
      rule.ops1List[Boolean](StdlibNumericTables.boolBitwise),
      rule.byName_LsList[Boolean](StdlibNumericTables.boolByName)
    )

  def char(rule: RulesFor[Char]): List[CallRule] =
    RuleHelpers.concat(
      charBasics(rule),
      rule.ops1IntList(StdlibNumericTables.charIntArith),
      rule.ops1List[Char](StdlibNumericTables.charCharArith)
    )

  def richChar(rule: RulesFor[Char]): List[CallRule] =
    RuleHelpers.concat(
      charBasics(rule),
      rule.ops1List[Char](StdlibNumericTables.charRange),
      rule.opsList(StdlibNumericTables.richCharMethods)
    )

  def intRange(rule: RulesFor[Int]): List[CallRule] =
    rule.ops1IntList(StdlibNumericTables.intRange)

  def richIntRange(rule: RulesFor[scala.runtime.RichInt]): List[CallRule] =
    rule.ops1IntList(StdlibNumericTables.richIntRange)

  def bigInt(rule: RulesFor[BigInt]): List[CallRule] =
    RuleHelpers.concat(
      rule.opsList(StdlibNumericTables.bigIntUnary),
      rule.opsList(StdlibNumericTables.bigIntConversions),
      rule.ops1List[BigInt](StdlibNumericTables.bigIntArith),
      rule.ops1List[BigInt](StdlibNumericTables.bigIntCompare),
      rule.ops1IntList(StdlibNumericTables.bigIntPow)
    )

  def bigDecimal(rule: RulesFor[BigDecimal]): List[CallRule] =
    RuleHelpers.concat(
      rule.opsList(StdlibNumericTables.bigDecimalUnary),
      rule.opsList(StdlibNumericTables.bigDecimalConversions),
      rule.ops1List[BigDecimal](StdlibNumericTables.bigDecimalArith),
      rule.ops1List[BigDecimal](StdlibNumericTables.bigDecimalCompare)
    )

private[comptime] object StdlibBigNumCtorRules:
  import RuleHelpers.*

  // BigInt companion constructors
  def bigIntCtor(rule: RulesFor[Any]): List[CallRule] =
    RuleHelpers.concat(
      rule.arg1sAny(
        "apply" -> ((v: Any) =>
          v match
            case i: Int     => BigInt(i)
            case l: Long    => BigInt(l)
            case s: String  => BigInt(s)
            case bi: BigInt => bi
            case _          => throw new RuntimeException(s"Unsupported BigInt constructor arg: $v")
        )
      )
    )

  // BigDecimal companion constructors
  def bigDecimalCtor(rule: RulesFor[Any]): List[CallRule] =
    rule.arg1sAny(
      "apply" -> ((v: Any) =>
        v match
          case i: Int         => BigDecimal(i)
          case l: Long        => BigDecimal(l)
          case d: Double      => BigDecimal(d)
          case s: String      => BigDecimal(s)
          case bd: BigDecimal => bd
          case bi: BigInt     => BigDecimal(bi)
          case _              => throw new RuntimeException(s"Unsupported BigDecimal constructor arg: $v")
      )
    )

object StdlibNumericRules:
  private val intRule       = RulesFor[Int](RuleDsl.int)
  private val longRule      = RulesFor[Long](RuleDsl.long)
  private val floatRule     = RulesFor[Float](RuleDsl.float)
  private val doubleRule    = RulesFor[Double](RuleDsl.double)
  private val boolRule      = RulesFor[Boolean](RuleDsl.bool)
  private val charRule      = RulesFor[Char](RuleDsl.char)
  private val byteRule      = RulesFor[Byte](RuleDsl.byte)
  private val shortRule     = RulesFor[Short](RuleDsl.short)
  private val richIntRule   = RulesFor[scala.runtime.RichInt](Recv.union("scala.runtime.RichInt"))
  private val richLongRule  = RulesFor[scala.runtime.RichLong](Recv.union("scala.runtime.RichLong"))
  private val richCharRule  = RulesFor[Char](Recv.union("scala.runtime.RichChar", "scala.runtime.IntegralProxy"))
  private val richByteRule  = RulesFor[Byte](Recv.union("scala.runtime.RichByte"))
  private val richShortRule = RulesFor[Short](Recv.union("scala.runtime.RichShort"))
  private val richFloatRule = RulesFor[Float](Recv.union("scala.runtime.RichFloat")) // Float for comparisons
  private val richFloatRule2 =
    RulesFor[scala.runtime.RichFloat](Recv.union("scala.runtime.RichFloat")) // RichFloat for min/max
  private val richDoubleRule = RulesFor[scala.runtime.RichDouble](Recv.union("scala.runtime.RichDouble"))

  // BigInt and BigDecimal receivers
  private val bigIntRecv     = Recv.union("scala.math.BigInt")
  private val bigDecimalRecv = Recv.union("scala.math.BigDecimal")
  // Ordered receiver for comparison ops - used with runtime type dispatch
  private val orderedRecv             = Recv.union("scala.math.Ordered")
  private val bigIntCompanionRecv     = Recv.modules("scala.math.BigInt", "scala.package$")
  private val bigDecimalCompanionRecv = Recv.modules("scala.math.BigDecimal", "scala.package$")

  private val bigIntRule              = RulesFor[BigInt](bigIntRecv)
  private val bigDecimalRule          = RulesFor[BigDecimal](bigDecimalRecv)
  private val bigIntCompanionRule     = RulesFor.any(bigIntCompanionRecv)
  private val bigDecimalCompanionRule = RulesFor.any(bigDecimalCompanionRecv)

  private val intRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibNumericHelpers.arithOps(intRule)(-_, _ + _, _ - _, _ * _, _ / _, _ % _),
      StdlibNumericHelpers.comparisonsPromoted(intRule), // Promotes to widest type
      StdlibNumericHelpers.arithOpsPromoted(intRule),    // Int + Double -> Double, etc.
      StdlibNumericBasicRules.intRange(intRule),
      intRule.ops1IntList(StdlibNumericTables.intBitwise),
      intRule.opsList(StdlibNumericTables.intConversions)
    )

  private val longRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibNumericHelpers.arithOps(longRule)(-_, _ + _, _ - _, _ * _, _ / _, _ % _),
      StdlibNumericHelpers.comparisonsPromoted(longRule), // Promotes to widest type
      StdlibNumericHelpers.arithOpsPromoted(longRule),    // Long + Double -> Double, etc.
      longRule.ops1IntList(StdlibNumericTables.longBitwise),
      longRule.ops1List[Long](StdlibNumericTables.longBitwiseLong),
      longRule.opsList(StdlibNumericTables.longConversions)
    )

  private val floatRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibNumericHelpers.arithOps(floatRule)(-_, _ + _, _ - _, _ * _, _ / _, _ % _),
      StdlibNumericHelpers.comparisonsPromoted(floatRule), // Promotes to widest type
      StdlibNumericHelpers.arithOpsPromoted(floatRule)     // Float + Double -> Double, etc.
    )

  private val doubleRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibNumericHelpers.arithOps(doubleRule)(-_, _ + _, _ - _, _ * _, _ / _, _ % _),
      StdlibNumericHelpers.comparisonsPromoted(doubleRule), // Double stays Double
      StdlibNumericHelpers.arithOpsPromoted(doubleRule)     // Double stays Double
    )

  private val boolRules: List[CallRule] =
    StdlibNumericBasicRules.bool(boolRule)

  private val charRules: List[CallRule] =
    StdlibNumericBasicRules.char(charRule)

  private val byteRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibNumericHelpers.comparisonsPromoted(byteRule),
      StdlibNumericHelpers.arithOpsPromoted(byteRule),
      byteRule.opsAnyRecvList(StdlibNumericTables.byteUnary)
    )

  private val shortRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibNumericHelpers.comparisonsPromoted(shortRule),
      StdlibNumericHelpers.arithOpsPromoted(shortRule),
      shortRule.opsAnyRecvList(StdlibNumericTables.shortUnary)
    )

  private val richCharRules: List[CallRule] =
    StdlibNumericBasicRules.richChar(richCharRule)

  private val richByteRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibNumericHelpers.comparisonsPromoted(richByteRule),
      richByteRule.opsAnyRecvList(StdlibNumericTables.richByteUnary)
    )

  private val richShortRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibNumericHelpers.comparisonsPromoted(richShortRule),
      richShortRule.opsAnyRecvList(StdlibNumericTables.richShortUnary)
    )

  private val richFloatRules: List[CallRule] =
    StdlibNumericHelpers.comparisonsPromoted(richFloatRule)

  private val richIntRules: List[CallRule] =
    RuleHelpers.concat(
      StdlibNumericBasicRules.richIntRange(richIntRule),
      richIntRule.ops1IntList(StdlibNumericTables.richIntMinMax)
    )

  private val richLongRules: List[CallRule] =
    richLongRule.ops1List[Long](StdlibNumericTables.richLongMinMax)

  private val richDoubleRules: List[CallRule] =
    richDoubleRule.ops1List[Double](StdlibNumericTables.richDoubleMinMax)

  private val richFloatMinMaxRules: List[CallRule] =
    richFloatRule2.ops1List[Float](StdlibNumericTables.richFloatMinMax)

  private val bigIntRules: List[CallRule] =
    StdlibNumericBasicRules.bigInt(bigIntRule)

  private val bigDecimalRules: List[CallRule] =
    StdlibNumericBasicRules.bigDecimal(bigDecimalRule)

  private val bigIntCtorRules: List[CallRule] =
    StdlibBigNumCtorRules.bigIntCtor(bigIntCompanionRule)

  private val bigDecimalCtorRules: List[CallRule] =
    StdlibBigNumCtorRules.bigDecimalCtor(bigDecimalCompanionRule)

  // Range operations - Range.by(step) for stepping
  private val rangeRecv = Recv.union("scala.collection.immutable.Range")
  private val rangeRule = RulesFor[Range](rangeRecv)

  private val rangeRules: List[CallRule] =
    rangeRule.ops1IntList(
      List(
        "by" -> ((range, step) => range.by(step))
      )
    )

  // Ordered comparison rules with runtime type dispatch
  // These handle <, <=, >, >= on BigInt and BigDecimal when owner is Ordered
  private val orderedCompareRules: List[CallRule] =
    import RuleHelpers.*
    val orderedRule = RulesFor.any(orderedRecv)
    orderedRule.ops1Any(
      "<" -> { (recv, other) =>
        (recv, other) match
          case (a: BigInt, b: BigInt)         => a < b
          case (a: BigDecimal, b: BigDecimal) => a < b
          case _                              => throw new RuntimeException(s"Unsupported Ordered.< for ${recv.getClass} and ${other.getClass}")
      },
      "<=" -> { (recv, other) =>
        (recv, other) match
          case (a: BigInt, b: BigInt)         => a <= b
          case (a: BigDecimal, b: BigDecimal) => a <= b
          case _                              => throw new RuntimeException(s"Unsupported Ordered.<= for ${recv.getClass} and ${other.getClass}")
      },
      ">" -> { (recv, other) =>
        (recv, other) match
          case (a: BigInt, b: BigInt)         => a > b
          case (a: BigDecimal, b: BigDecimal) => a > b
          case _                              => throw new RuntimeException(s"Unsupported Ordered.> for ${recv.getClass} and ${other.getClass}")
      },
      ">=" -> { (recv, other) =>
        (recv, other) match
          case (a: BigInt, b: BigInt)         => a >= b
          case (a: BigDecimal, b: BigDecimal) => a >= b
          case _                              => throw new RuntimeException(s"Unsupported Ordered.>= for ${recv.getClass} and ${other.getClass}")
      }
    )

  val rules: List[CallRule] =
    RuleHelpers.concat(
      intRules,
      longRules,
      floatRules,
      doubleRules,
      boolRules,
      charRules,
      byteRules,
      shortRules,
      richIntRules,
      richLongRules,
      richCharRules,
      richByteRules,
      richShortRules,
      richFloatRules,
      richFloatMinMaxRules,
      richDoubleRules,
      bigDecimalRules,
      bigIntRules,
      orderedCompareRules,
      bigDecimalCtorRules,
      bigIntCtorRules,
      rangeRules
    )
