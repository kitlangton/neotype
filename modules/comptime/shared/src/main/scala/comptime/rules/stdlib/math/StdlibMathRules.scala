// Math static methods (java.lang.Math and scala.math)
package comptime

object StdlibMathRules:
  import RuleHelpers.*

  // scala.math package object methods dispatch to java.lang.Math
  private val mathRecv = Recv.modules(
    "scala.math.package",
    "scala.math.package$",
    "java.lang.Math"
  )

  private val mathRule = RulesFor.any(mathRecv)

  // For methods that need type-specific dispatch, we use runtime pattern matching
  private def absRule: List[CallRule] =
    mathRule.arg1sAny(
      "abs" -> { (arg: Any) =>
        arg match
          case d: Double => Math.abs(d)
          case f: Float  => Math.abs(f)
          case l: Long   => Math.abs(l)
          case i: Int    => Math.abs(i)
          case _         => throw new RuntimeException(s"Unsupported abs argument: ${arg.getClass}")
      }
    )

  // max/min are static methods with 2 args - use arg2
  private def maxRule: CallRule =
    mathRule.arg2[Any, Any, Any]("max") { (a, b) =>
      (a, b) match
        case (a: Double, b: Double) => Math.max(a, b)
        case (a: Float, b: Float)   => Math.max(a, b)
        case (a: Long, b: Long)     => Math.max(a, b)
        case (a: Int, b: Int)       => Math.max(a, b)
        case _                      => throw new RuntimeException(s"Unsupported max arguments: ${a.getClass}, ${b.getClass}")
    }

  private def minRule: CallRule =
    mathRule.arg2[Any, Any, Any]("min") { (a, b) =>
      (a, b) match
        case (a: Double, b: Double) => Math.min(a, b)
        case (a: Float, b: Float)   => Math.min(a, b)
        case (a: Long, b: Long)     => Math.min(a, b)
        case (a: Int, b: Int)       => Math.min(a, b)
        case _                      => throw new RuntimeException(s"Unsupported min arguments: ${a.getClass}, ${b.getClass}")
    }

  private def signumRule: List[CallRule] =
    mathRule.arg1sAny(
      "signum" -> { (arg: Any) =>
        arg match
          case d: Double => Math.signum(d)
          case f: Float  => Math.signum(f)
          case l: Long   => java.lang.Long.signum(l)
          case i: Int    => Integer.signum(i)
          case _         => throw new RuntimeException(s"Unsupported signum argument: ${arg.getClass}")
      }
    )

  private def roundRule: List[CallRule] =
    mathRule.arg1sAny(
      "round" -> { (arg: Any) =>
        arg match
          case d: Double => Math.round(d)
          case f: Float  => Math.round(f)
          case _         => throw new RuntimeException(s"Unsupported round argument: ${arg.getClass}")
      }
    )

  // Double-only unary functions - use arg1sAnyList with type dispatch
  private val doubleUnaryRules: List[CallRule] =
    mathRule.arg1sAnyList(
      List(
        "sqrt"      -> ((x: Any) => Math.sqrt(x.asInstanceOf[Double])),
        "cbrt"      -> ((x: Any) => Math.cbrt(x.asInstanceOf[Double])),
        "ceil"      -> ((x: Any) => Math.ceil(x.asInstanceOf[Double])),
        "floor"     -> ((x: Any) => Math.floor(x.asInstanceOf[Double])),
        "sin"       -> ((x: Any) => Math.sin(x.asInstanceOf[Double])),
        "cos"       -> ((x: Any) => Math.cos(x.asInstanceOf[Double])),
        "tan"       -> ((x: Any) => Math.tan(x.asInstanceOf[Double])),
        "asin"      -> ((x: Any) => Math.asin(x.asInstanceOf[Double])),
        "acos"      -> ((x: Any) => Math.acos(x.asInstanceOf[Double])),
        "atan"      -> ((x: Any) => Math.atan(x.asInstanceOf[Double])),
        "sinh"      -> ((x: Any) => Math.sinh(x.asInstanceOf[Double])),
        "cosh"      -> ((x: Any) => Math.cosh(x.asInstanceOf[Double])),
        "tanh"      -> ((x: Any) => Math.tanh(x.asInstanceOf[Double])),
        "exp"       -> ((x: Any) => Math.exp(x.asInstanceOf[Double])),
        "log"       -> ((x: Any) => Math.log(x.asInstanceOf[Double])),
        "log10"     -> ((x: Any) => Math.log10(x.asInstanceOf[Double])),
        "toDegrees" -> ((x: Any) => Math.toDegrees(x.asInstanceOf[Double])),
        "toRadians" -> ((x: Any) => Math.toRadians(x.asInstanceOf[Double]))
      )
    )

  // Double binary functions - static 2-arg methods
  private val doubleBinaryRules: List[CallRule] =
    List(
      mathRule.arg2[Double, Double, Double]("pow")(Math.pow(_, _)),
      mathRule.arg2[Double, Double, Double]("atan2")(Math.atan2(_, _)),
      mathRule.arg2[Double, Double, Double]("hypot")(Math.hypot(_, _))
    )

  val rules: List[CallRule] =
    RuleHelpers.concat(
      absRule,
      List(maxRule, minRule),
      signumRule,
      roundRule,
      doubleUnaryRules,
      doubleBinaryRules
    )
