package comptime

import scala.quoted.*

private[comptime] object ScalaAstBridgeNormalize:
  private def castTypedLiteral[Q <: Quotes](using
      quotes: Q
  )(value: Any, tpe: quotes.reflect.TypeRepr): Option[Any] =
    import quotes.reflect.*
    def toByte(v: Any): Option[Byte] = v match
      case b: Byte   => Some(b)
      case s: Short  => Some(s.toByte)
      case i: Int    => Some(i.toByte)
      case l: Long   => Some(l.toByte)
      case f: Float  => Some(f.toByte)
      case d: Double => Some(d.toByte)
      case _         => None
    def toShort(v: Any): Option[Short] = v match
      case b: Byte   => Some(b.toShort)
      case s: Short  => Some(s)
      case i: Int    => Some(i.toShort)
      case l: Long   => Some(l.toShort)
      case f: Float  => Some(f.toShort)
      case d: Double => Some(d.toShort)
      case _         => None
    def toChar(v: Any): Option[Char] = v match
      case c: Char  => Some(c)
      case b: Byte  => Some(b.toChar)
      case s: Short => Some(s.toChar)
      case i: Int   => Some(i.toChar)
      case l: Long  => Some(l.toChar)
      case _        => None
    def toInt(v: Any): Option[Int] = v match
      case b: Byte   => Some(b.toInt)
      case s: Short  => Some(s.toInt)
      case c: Char   => Some(c.toInt)
      case i: Int    => Some(i)
      case l: Long   => Some(l.toInt)
      case f: Float  => Some(f.toInt)
      case d: Double => Some(d.toInt)
      case _         => None
    def toLong(v: Any): Option[Long] = v match
      case b: Byte   => Some(b.toLong)
      case s: Short  => Some(s.toLong)
      case c: Char   => Some(c.toLong)
      case i: Int    => Some(i.toLong)
      case l: Long   => Some(l)
      case f: Float  => Some(f.toLong)
      case d: Double => Some(d.toLong)
      case _         => None
    def toFloat(v: Any): Option[Float] = v match
      case b: Byte   => Some(b.toFloat)
      case s: Short  => Some(s.toFloat)
      case c: Char   => Some(c.toFloat)
      case i: Int    => Some(i.toFloat)
      case l: Long   => Some(l.toFloat)
      case f: Float  => Some(f)
      case d: Double => Some(d.toFloat)
      case _         => None
    def toDouble(v: Any): Option[Double] = v match
      case b: Byte   => Some(b.toDouble)
      case s: Short  => Some(s.toDouble)
      case c: Char   => Some(c.toDouble)
      case i: Int    => Some(i.toDouble)
      case l: Long   => Some(l.toDouble)
      case f: Float  => Some(f.toDouble)
      case d: Double => Some(d)
      case _         => None

    val dealised = tpe.dealias
    if dealised =:= TypeRepr.of[Byte] then toByte(value)
    else if dealised =:= TypeRepr.of[Short] then toShort(value)
    else if dealised =:= TypeRepr.of[Char] then toChar(value)
    else if dealised =:= TypeRepr.of[Int] then toInt(value)
    else if dealised =:= TypeRepr.of[Long] then toLong(value)
    else if dealised =:= TypeRepr.of[Float] then toFloat(value)
    else if dealised =:= TypeRepr.of[Double] then toDouble(value)
    else None

  def normalize[Q <: Quotes](using quotes: Q)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case Inlined(_, bindings, t) =>
        if bindings.nonEmpty then normalize(Block(bindings, t)) else normalize(t)
      case Typed(t, tpt) =>
        val normalized = normalize(t)
        normalized match
          case Literal(constant) =>
            castTypedLiteral(constant.value, tpt.tpe) match
              case Some(b: Byte)   => Literal(ByteConstant(b))
              case Some(s: Short)  => Literal(ShortConstant(s))
              case Some(c: Char)   => Literal(CharConstant(c))
              case Some(i: Int)    => Literal(IntConstant(i))
              case Some(l: Long)   => Literal(LongConstant(l))
              case Some(f: Float)  => Literal(FloatConstant(f))
              case Some(d: Double) => Literal(DoubleConstant(d))
              case _               => normalized
          case _ =>
            normalized
      case lit @ Literal(constant) =>
        // Use widen to get the declared type (e.g., Byte) not ConstantType(5)
        castTypedLiteral(constant.value, lit.tpe.widen) match
          case Some(b: Byte)   => Literal(ByteConstant(b))
          case Some(s: Short)  => Literal(ShortConstant(s))
          case Some(c: Char)   => Literal(CharConstant(c))
          case Some(i: Int)    => Literal(IntConstant(i))
          case Some(l: Long)   => Literal(LongConstant(l))
          case Some(f: Float)  => Literal(FloatConstant(f))
          case Some(d: Double) => Literal(DoubleConstant(d))
          case _               => lit
      case Block(Nil, t) => normalize(t)
      case _             => term
