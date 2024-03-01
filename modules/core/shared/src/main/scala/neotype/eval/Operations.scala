package neotype.eval

private[neotype] object Operations:

  def minus(lhs: Any, rhs: Any): Any =
    (lhs, rhs) match
      case (lhs: Set[Any], rhs: Any) => lhs - rhs
      case _                         => performNumericBinOp(NumericBinOp.Minus, lhs, rhs)
  def plus(lhs: Any, rhs: Any): Any =
    (lhs, rhs) match
      case (lhs: String, rhs: String) => lhs + rhs
      case (lhs: Set[Any], rhs: Any)  => lhs + rhs
      case _                          => performNumericBinOp(NumericBinOp.Plus, lhs, rhs)
  def times(lhs: Any, rhs: Any): Any  = performNumericBinOp(NumericBinOp.Times, lhs, rhs)
  def divide(lhs: Any, rhs: Any): Any = performNumericBinOp(NumericBinOp.Divide, lhs, rhs)
  def mod(lhs: Any, rhs: Any): Any    = performNumericBinOp(NumericBinOp.Mod, lhs, rhs)
  def pow(lhs: Any, rhs: Any): Any    = performNumericBinOp(NumericBinOp.Pow, lhs, rhs)
  def min(lhs: Any, rhs: Any): Any    = performNumericBinOp(NumericBinOp.Min, lhs, rhs)
  def max(lhs: Any, rhs: Any): Any    = performNumericBinOp(NumericBinOp.Max, lhs, rhs)
  def lessThan(lhs: Any, rhs: Any): Any =
    (lhs, rhs) match
      case (lhs: String, rhs: String) => lhs < rhs
      case _                          => performNumericBinOp(NumericBinOp.LessThan, lhs, rhs)

  def lessThanOrEqual(lhs: Any, rhs: Any): Any =
    (lhs, rhs) match
      case (lhs: String, rhs: String) => lhs <= rhs
      case _                          => performNumericBinOp(NumericBinOp.LessThanOrEqual, lhs, rhs)

  def greaterThan(lhs: Any, rhs: Any): Any =
    (lhs, rhs) match
      case (lhs: String, rhs: String) => lhs > rhs
      case _                          => performNumericBinOp(NumericBinOp.GreaterThan, lhs, rhs)

  def greaterThanOrEqual(lhs: Any, rhs: Any): Any =
    (lhs, rhs) match
      case (lhs: String, rhs: String) => lhs >= rhs
      case _                          => performNumericBinOp(NumericBinOp.GreaterThanOrEqual, lhs, rhs)

  // Adding (1: Byte) to (1: Int) will fail with a ClassCastException,
  // so we need to convert the Byte to an Int before adding.
  // We need to always convert to the larger type, so we need to know
  // the type of the larger type.
  private def performNumericBinOp(op: NumericBinOp, lhs: Any, rhs: Any): Any =
    val lhsType    = NumericType.forNumber(lhs)
    val rhsType    = NumericType.forNumber(rhs)
    val widestType = lhsType.widest(rhsType)
    val f          = NumericBinOp.ops((widestType, op))
    f(widestType.widen(lhs), widestType.widen(rhs))

private enum NumericBinOp:
  case Plus, Minus, Times, Divide, Mod, Min, Max, Pow, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual

private object NumericBinOp:
  val ops: Map[(NumericType, NumericBinOp), (Any, Any) => Any] =
    Map(
      // byte ops
      (NumericType.ByteT, NumericBinOp.Plus)               -> ((a: Byte, b: Byte) => a + b),
      (NumericType.ByteT, NumericBinOp.Minus)              -> ((a: Byte, b: Byte) => a - b),
      (NumericType.ByteT, NumericBinOp.Times)              -> ((a: Byte, b: Byte) => a * b),
      (NumericType.ByteT, NumericBinOp.Divide)             -> ((a: Byte, b: Byte) => a / b),
      (NumericType.ByteT, NumericBinOp.Mod)                -> ((a: Byte, b: Byte) => a % b),
      (NumericType.ByteT, NumericBinOp.Min)                -> ((a: Byte, b: Byte) => a.min(b)),
      (NumericType.ByteT, NumericBinOp.Max)                -> ((a: Byte, b: Byte) => a.max(b)),
      (NumericType.ByteT, NumericBinOp.Pow)                -> ((a: Byte, b: Byte) => Math.pow(a, b)),
      (NumericType.ByteT, NumericBinOp.LessThan)           -> ((a: Byte, b: Byte) => a < b),
      (NumericType.ByteT, NumericBinOp.LessThanOrEqual)    -> ((a: Byte, b: Byte) => a <= b),
      (NumericType.ByteT, NumericBinOp.GreaterThan)        -> ((a: Byte, b: Byte) => a > b),
      (NumericType.ByteT, NumericBinOp.GreaterThanOrEqual) -> ((a: Byte, b: Byte) => a >= b),

      // short ops
      (NumericType.ShortT, NumericBinOp.Plus)               -> ((a: Short, b: Short) => a + b),
      (NumericType.ShortT, NumericBinOp.Minus)              -> ((a: Short, b: Short) => a - b),
      (NumericType.ShortT, NumericBinOp.Times)              -> ((a: Short, b: Short) => a * b),
      (NumericType.ShortT, NumericBinOp.Divide)             -> ((a: Short, b: Short) => a / b),
      (NumericType.ShortT, NumericBinOp.Mod)                -> ((a: Short, b: Short) => a % b),
      (NumericType.ShortT, NumericBinOp.Min)                -> ((a: Short, b: Short) => a.min(b)),
      (NumericType.ShortT, NumericBinOp.Max)                -> ((a: Short, b: Short) => a.max(b)),
      (NumericType.ShortT, NumericBinOp.Pow)                -> ((a: Short, b: Short) => Math.pow(a, b)),
      (NumericType.ShortT, NumericBinOp.LessThan)           -> ((a: Short, b: Short) => a < b),
      (NumericType.ShortT, NumericBinOp.LessThanOrEqual)    -> ((a: Short, b: Short) => a <= b),
      (NumericType.ShortT, NumericBinOp.GreaterThan)        -> ((a: Short, b: Short) => a > b),
      (NumericType.ShortT, NumericBinOp.GreaterThanOrEqual) -> ((a: Short, b: Short) => a >= b),
      // char ops
      (NumericType.CharT, NumericBinOp.Plus)               -> ((a: Char, b: Char) => a + b),
      (NumericType.CharT, NumericBinOp.Minus)              -> ((a: Char, b: Char) => a - b),
      (NumericType.CharT, NumericBinOp.Times)              -> ((a: Char, b: Char) => a * b),
      (NumericType.CharT, NumericBinOp.Divide)             -> ((a: Char, b: Char) => a / b),
      (NumericType.CharT, NumericBinOp.Mod)                -> ((a: Char, b: Char) => a % b),
      (NumericType.CharT, NumericBinOp.Min)                -> ((a: Char, b: Char) => a.min(b)),
      (NumericType.CharT, NumericBinOp.Max)                -> ((a: Char, b: Char) => a.max(b)),
      (NumericType.CharT, NumericBinOp.Pow)                -> ((a: Char, b: Char) => Math.pow(a, b)),
      (NumericType.CharT, NumericBinOp.LessThan)           -> ((a: Char, b: Char) => a < b),
      (NumericType.CharT, NumericBinOp.LessThanOrEqual)    -> ((a: Char, b: Char) => a <= b),
      (NumericType.CharT, NumericBinOp.GreaterThan)        -> ((a: Char, b: Char) => a > b),
      (NumericType.CharT, NumericBinOp.GreaterThanOrEqual) -> ((a: Char, b: Char) => a >= b),
      // int ops
      (NumericType.IntT, NumericBinOp.Plus)               -> ((a: Int, b: Int) => a + b),
      (NumericType.IntT, NumericBinOp.Minus)              -> ((a: Int, b: Int) => a - b),
      (NumericType.IntT, NumericBinOp.Times)              -> ((a: Int, b: Int) => a * b),
      (NumericType.IntT, NumericBinOp.Divide)             -> ((a: Int, b: Int) => a / b),
      (NumericType.IntT, NumericBinOp.Mod)                -> ((a: Int, b: Int) => a % b),
      (NumericType.IntT, NumericBinOp.Min)                -> ((a: Int, b: Int) => a.min(b)),
      (NumericType.IntT, NumericBinOp.Max)                -> ((a: Int, b: Int) => a.max(b)),
      (NumericType.IntT, NumericBinOp.Pow)                -> ((a: Int, b: Int) => Math.pow(a, b)),
      (NumericType.IntT, NumericBinOp.LessThan)           -> ((a: Int, b: Int) => a < b),
      (NumericType.IntT, NumericBinOp.LessThanOrEqual)    -> ((a: Int, b: Int) => a <= b),
      (NumericType.IntT, NumericBinOp.GreaterThan)        -> ((a: Int, b: Int) => a > b),
      (NumericType.IntT, NumericBinOp.GreaterThanOrEqual) -> ((a: Int, b: Int) => a >= b),
      // long ops
      (NumericType.LongT, NumericBinOp.Plus)               -> ((a: Long, b: Long) => a + b),
      (NumericType.LongT, NumericBinOp.Minus)              -> ((a: Long, b: Long) => a - b),
      (NumericType.LongT, NumericBinOp.Times)              -> ((a: Long, b: Long) => a * b),
      (NumericType.LongT, NumericBinOp.Divide)             -> ((a: Long, b: Long) => a / b),
      (NumericType.LongT, NumericBinOp.Mod)                -> ((a: Long, b: Long) => a % b),
      (NumericType.LongT, NumericBinOp.Min)                -> ((a: Long, b: Long) => a.min(b)),
      (NumericType.LongT, NumericBinOp.Max)                -> ((a: Long, b: Long) => a.max(b)),
      (NumericType.LongT, NumericBinOp.Pow)                -> ((a: Long, b: Long) => Math.pow(a, b)),
      (NumericType.LongT, NumericBinOp.LessThan)           -> ((a: Long, b: Long) => a < b),
      (NumericType.LongT, NumericBinOp.LessThanOrEqual)    -> ((a: Long, b: Long) => a <= b),
      (NumericType.LongT, NumericBinOp.GreaterThan)        -> ((a: Long, b: Long) => a > b),
      (NumericType.LongT, NumericBinOp.GreaterThanOrEqual) -> ((a: Long, b: Long) => a >= b),
      // float ops
      (NumericType.FloatT, NumericBinOp.Plus)               -> ((a: Float, b: Float) => a + b),
      (NumericType.FloatT, NumericBinOp.Minus)              -> ((a: Float, b: Float) => a - b),
      (NumericType.FloatT, NumericBinOp.Times)              -> ((a: Float, b: Float) => a * b),
      (NumericType.FloatT, NumericBinOp.Divide)             -> ((a: Float, b: Float) => a / b),
      (NumericType.FloatT, NumericBinOp.Mod)                -> ((a: Float, b: Float) => a % b),
      (NumericType.FloatT, NumericBinOp.Min)                -> ((a: Float, b: Float) => a.min(b)),
      (NumericType.FloatT, NumericBinOp.Max)                -> ((a: Float, b: Float) => a.max(b)),
      (NumericType.FloatT, NumericBinOp.Pow)                -> ((a: Float, b: Float) => Math.pow(a, b)),
      (NumericType.FloatT, NumericBinOp.LessThan)           -> ((a: Float, b: Float) => a < b),
      (NumericType.FloatT, NumericBinOp.LessThanOrEqual)    -> ((a: Float, b: Float) => a <= b),
      (NumericType.FloatT, NumericBinOp.GreaterThan)        -> ((a: Float, b: Float) => a > b),
      (NumericType.FloatT, NumericBinOp.GreaterThanOrEqual) -> ((a: Float, b: Float) => a >= b),
      // double ops
      (NumericType.DoubleT, NumericBinOp.Plus)               -> ((a: Double, b: Double) => a + b),
      (NumericType.DoubleT, NumericBinOp.Minus)              -> ((a: Double, b: Double) => a - b),
      (NumericType.DoubleT, NumericBinOp.Times)              -> ((a: Double, b: Double) => a * b),
      (NumericType.DoubleT, NumericBinOp.Divide)             -> ((a: Double, b: Double) => a / b),
      (NumericType.DoubleT, NumericBinOp.Mod)                -> ((a: Double, b: Double) => a % b),
      (NumericType.DoubleT, NumericBinOp.Min)                -> ((a: Double, b: Double) => a.min(b)),
      (NumericType.DoubleT, NumericBinOp.Max)                -> ((a: Double, b: Double) => a.max(b)),
      (NumericType.DoubleT, NumericBinOp.Pow)                -> ((a: Double, b: Double) => Math.pow(a, b)),
      (NumericType.DoubleT, NumericBinOp.LessThan)           -> ((a: Double, b: Double) => a < b),
      (NumericType.DoubleT, NumericBinOp.LessThanOrEqual)    -> ((a: Double, b: Double) => a <= b),
      (NumericType.DoubleT, NumericBinOp.GreaterThan)        -> ((a: Double, b: Double) => a > b),
      (NumericType.DoubleT, NumericBinOp.GreaterThanOrEqual) -> ((a: Double, b: Double) => a >= b),
      // big int ops
      (NumericType.BigIntT, NumericBinOp.Plus)               -> ((a: BigInt, b: BigInt) => a + b),
      (NumericType.BigIntT, NumericBinOp.Minus)              -> ((a: BigInt, b: BigInt) => a - b),
      (NumericType.BigIntT, NumericBinOp.Times)              -> ((a: BigInt, b: BigInt) => a * b),
      (NumericType.BigIntT, NumericBinOp.Divide)             -> ((a: BigInt, b: BigInt) => a / b),
      (NumericType.BigIntT, NumericBinOp.Mod)                -> ((a: BigInt, b: BigInt) => a % b),
      (NumericType.BigIntT, NumericBinOp.Min)                -> ((a: BigInt, b: BigInt) => a.min(b)),
      (NumericType.BigIntT, NumericBinOp.Max)                -> ((a: BigInt, b: BigInt) => a.max(b)),
      (NumericType.BigIntT, NumericBinOp.Pow)                -> ((a: BigInt, b: BigInt) => a.pow(b.toInt)),
      (NumericType.BigIntT, NumericBinOp.LessThan)           -> ((a: BigInt, b: BigInt) => a < b),
      (NumericType.BigIntT, NumericBinOp.LessThanOrEqual)    -> ((a: BigInt, b: BigInt) => a <= b),
      (NumericType.BigIntT, NumericBinOp.GreaterThan)        -> ((a: BigInt, b: BigInt) => a > b),
      (NumericType.BigIntT, NumericBinOp.GreaterThanOrEqual) -> ((a: BigInt, b: BigInt) => a >= b),
      // big decimal ops
      (NumericType.BigDecimalT, NumericBinOp.Plus)               -> ((a: BigDecimal, b: BigDecimal) => a + b),
      (NumericType.BigDecimalT, NumericBinOp.Minus)              -> ((a: BigDecimal, b: BigDecimal) => a - b),
      (NumericType.BigDecimalT, NumericBinOp.Times)              -> ((a: BigDecimal, b: BigDecimal) => a * b),
      (NumericType.BigDecimalT, NumericBinOp.Divide)             -> ((a: BigDecimal, b: BigDecimal) => a / b),
      (NumericType.BigDecimalT, NumericBinOp.Mod)                -> ((a: BigDecimal, b: BigDecimal) => a % b),
      (NumericType.BigDecimalT, NumericBinOp.Min)                -> ((a: BigDecimal, b: BigDecimal) => a.min(b)),
      (NumericType.BigDecimalT, NumericBinOp.Max)                -> ((a: BigDecimal, b: BigDecimal) => a.max(b)),
      (NumericType.BigDecimalT, NumericBinOp.Pow)                -> ((a: BigDecimal, b: BigDecimal) => a.pow(b.toInt)),
      (NumericType.BigDecimalT, NumericBinOp.LessThan)           -> ((a: BigDecimal, b: BigDecimal) => a < b),
      (NumericType.BigDecimalT, NumericBinOp.LessThanOrEqual)    -> ((a: BigDecimal, b: BigDecimal) => a <= b),
      (NumericType.BigDecimalT, NumericBinOp.GreaterThan)        -> ((a: BigDecimal, b: BigDecimal) => a > b),
      (NumericType.BigDecimalT, NumericBinOp.GreaterThanOrEqual) -> ((a: BigDecimal, b: BigDecimal) => a >= b)
    ).asInstanceOf[Map[(NumericType, NumericBinOp), (Any, Any) => Any]]

private enum NumericType:
  // from narrowest to widest
  case ByteT, ShortT, CharT, IntT, LongT, FloatT, DoubleT, BigIntT, BigDecimalT

  def widest(that: NumericType): NumericType =
    if this.ordinal > that.ordinal then this else that

  def widen(any: Any): Any =
    (any, this) match
      case (any: Byte, ByteT)       => any
      case (any: Byte, ShortT)      => any.toShort
      case (any: Byte, CharT)       => any.toChar
      case (any: Byte, IntT)        => any.toInt
      case (any: Byte, LongT)       => any.toLong
      case (any: Byte, FloatT)      => any.toFloat
      case (any: Byte, DoubleT)     => any.toDouble
      case (any: Byte, BigIntT)     => BigInt(any)
      case (any: Byte, BigDecimalT) => BigDecimal(any)

      case (any: Short, ShortT)      => any
      case (any: Short, CharT)       => any.toChar
      case (any: Short, IntT)        => any.toInt
      case (any: Short, LongT)       => any.toLong
      case (any: Short, FloatT)      => any.toFloat
      case (any: Short, DoubleT)     => any.toDouble
      case (any: Short, BigIntT)     => BigInt(any)
      case (any: Short, BigDecimalT) => BigDecimal(any)

      case (any: Char, CharT)       => any
      case (any: Char, IntT)        => any.toInt
      case (any: Char, LongT)       => any.toLong
      case (any: Char, FloatT)      => any.toFloat
      case (any: Char, DoubleT)     => any.toDouble
      case (any: Char, BigIntT)     => BigInt(any)
      case (any: Char, BigDecimalT) => BigDecimal(any)

      case (any: Int, IntT)        => any
      case (any: Int, LongT)       => any.toLong
      case (any: Int, FloatT)      => any.toFloat
      case (any: Int, DoubleT)     => any.toDouble
      case (any: Int, BigIntT)     => BigInt(any)
      case (any: Int, BigDecimalT) => BigDecimal(any)

      case (any: Long, LongT)       => any
      case (any: Long, FloatT)      => any.toFloat
      case (any: Long, DoubleT)     => any.toDouble
      case (any: Long, BigIntT)     => BigInt(any)
      case (any: Long, BigDecimalT) => BigDecimal(any)

      case (any: Float, FloatT)      => any
      case (any: Float, DoubleT)     => any.toDouble
      case (any: Float, BigDecimalT) => BigDecimal(any)

      case (any: Double, DoubleT)     => any
      case (any: Double, BigDecimalT) => BigDecimal(any)

      case (any: BigInt, BigIntT)     => any
      case (any: BigInt, BigDecimalT) => BigDecimal(any)

private object NumericType:
  def forNumber(any: Any): NumericType =
    any match
      case _: Int        => NumericType.IntT
      case _: Long       => NumericType.LongT
      case _: Short      => NumericType.ShortT
      case _: Char       => NumericType.CharT
      case _: Byte       => NumericType.ByteT
      case _: Double     => NumericType.DoubleT
      case _: Float      => NumericType.FloatT
      case _: BigInt     => NumericType.BigIntT
      case _: BigDecimal => NumericType.BigDecimalT
      case _             => throw new IllegalArgumentException(s"Cannot find numeric for ${any}")
