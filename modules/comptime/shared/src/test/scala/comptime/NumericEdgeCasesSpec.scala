package comptime

import zio.test.*

import scala.util.Try

// ═══════════════════════════════════════════════════════════════════════════
// NUMERIC EDGE CASES - IEEE 754, overflow, modulo, conversions
// ═══════════════════════════════════════════════════════════════════════════
//
// Tests for nuanced numeric behavior that should match Scala/JVM semantics.
//
object NumericEdgeCasesSpec extends ZIOSpecDefault:
  val spec = suite("NumericEdgeCasesSpec")(
    // ─────────────────────────────────────────────────────────────────────────
    // IEEE 754 SPECIAL VALUES - NaN, Infinity, -0.0
    // ─────────────────────────────────────────────────────────────────────────
    // Note: NaN equality (== and !=) uses boxed semantics in comptime, which
    // differs from IEEE 754. NaN comparisons (<, >, <=, >=) work correctly.
    // This is a known limitation due to how Any.== works in Scala.
    // ─────────────────────────────────────────────────────────────────────────
    suite("IEEE 754 special values")(
      suite("NaN behavior")(
        // Note: NaN == NaN returns true in comptime (boxed semantics)
        // In IEEE 754 it would be false, but boxed Double.equals returns true
        test("NaN < 0.0 is false") {
          assertTrue(comptime {
            val n = 0.0 / 0.0
            n < 0.0
          } == false)
        },
        test("NaN > 0.0 is false") {
          assertTrue(comptime {
            val n = 0.0 / 0.0
            n > 0.0
          } == false)
        },
        test("NaN >= 0.0 is false") {
          assertTrue(comptime {
            val n = 0.0 / 0.0
            n >= 0.0
          } == false)
        },
        test("NaN <= 0.0 is false") {
          assertTrue(comptime {
            val n = 0.0 / 0.0
            n <= 0.0
          } == false)
        },
        test("NaN arithmetic produces NaN (comparisons return false)") {
          assertTrue(comptime {
            val n      = 0.0 / 0.0
            val result = n + 1.0
            result < 0.0 || result > 0.0 // Both false for NaN
          } == false)
        }
      ),
      suite("Infinity behavior")(
        test("1.0 / 0.0 is positive infinity") {
          assertTrue(comptime {
            val inf = 1.0 / 0.0
            inf > 1.0e308
          } == true)
        },
        test("-1.0 / 0.0 is negative infinity") {
          assertTrue(comptime {
            val negInf = -1.0 / 0.0
            negInf < -1.0e308
          } == true)
        },
        test("Infinity == Infinity is true") {
          assertTrue(comptime {
            val inf1 = 1.0 / 0.0
            val inf2 = 1.0 / 0.0
            inf1 == inf2
          } == true)
        },
        test("Infinity + 1 is still Infinity") {
          assertTrue(comptime {
            val inf = 1.0 / 0.0
            (inf + 1.0) == inf
          } == true)
        },
        test("Infinity - Infinity produces NaN (comparisons return false)") {
          assertTrue(comptime {
            val inf    = 1.0 / 0.0
            val result = inf - inf
            result < 0.0 || result > 0.0 // Both false for NaN
          } == false)
        }
      ),
      suite("Negative zero behavior")(
        test("-0.0 == 0.0 is true") {
          assertTrue(comptime(-0.0 == 0.0) == true)
        },
        test("1.0 / -0.0 is negative infinity") {
          assertTrue(comptime {
            val negInf = 1.0 / -0.0
            negInf < -1.0e308
          } == true)
        },
        test("-0.0 + 0.0 == 0.0") {
          assertTrue(comptime(-0.0 + 0.0 == 0.0) == true)
        }
      )
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // INTEGER OVERFLOW/UNDERFLOW
    // ─────────────────────────────────────────────────────────────────────────
    suite("Integer overflow/underflow")(
      test("Int.MaxValue + 1 wraps to Int.MinValue") {
        assertTrue(comptime(2147483647 + 1) == -2147483648)
      },
      test("Int.MinValue - 1 wraps to Int.MaxValue") {
        assertTrue(comptime(-2147483648 - 1) == 2147483647)
      },
      test("Long.MaxValue + 1 wraps to Long.MinValue") {
        assertTrue(comptime(9223372036854775807L + 1L) == -9223372036854775808L)
      },
      test("Long.MinValue - 1 wraps to Long.MaxValue") {
        assertTrue(comptime(-9223372036854775808L - 1L) == 9223372036854775807L)
      },
      test("Int.MinValue / -1 overflows (wraps to Int.MinValue)") {
        assertTrue(comptime(-2147483648 / -1) == -2147483648)
      },
      test("Int overflow in multiplication") {
        assertTrue(comptime(100000 * 100000) == 1410065408) // overflows
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // NEGATIVE MODULO SEMANTICS
    // ─────────────────────────────────────────────────────────────────────────
    suite("Negative modulo")(
      test("-5 % 2 == -1") {
        assertTrue(comptime(-5 % 2) == -1)
      },
      test("5 % -2 == 1") {
        assertTrue(comptime(5 % -2) == 1)
      },
      test("-5 % -2 == -1") {
        assertTrue(comptime(-5 % -2) == -1)
      },
      test("-5.5 % 2.0 == -1.5") {
        assertTrue(comptime(-5.5 % 2.0) == -1.5)
      },
      test("5.5 % -2.0 == 1.5") {
        assertTrue(comptime(5.5 % -2.0) == 1.5)
      },
      test("-7L % 3L == -1L") {
        assertTrue(comptime(-7L % 3L) == -1L)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // DIVISION/MODULO BY ZERO
    // ─────────────────────────────────────────────────────────────────────────
    suite("Division and modulo by zero")(
      test("Int modulo by zero throws ArithmeticException") {
        assertTrue(comptime(Try(1 % 0).isFailure) == true)
      },
      test("Long modulo by zero throws ArithmeticException") {
        assertTrue(comptime(Try(1L % 0L).isFailure) == true)
      },
      test("Double division by zero returns Infinity") {
        assertTrue(comptime {
          val result = 1.0 / 0.0
          result > 1.0e308
        } == true)
      },
      test("Double modulo by zero returns NaN (comparisons false)") {
        assertTrue(comptime {
          val result = 1.0 % 0.0
          result < 0.0 || result > 0.0 // Both false for NaN
        } == false)
      },
      test("Float division by zero returns Infinity") {
        assertTrue(comptime {
          val result = 1.0f / 0.0f
          result > 1.0e38f
        } == true)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // LONG/FLOAT MIXED OPERATIONS
    // ─────────────────────────────────────────────────────────────────────────
    suite("Long and Float mixed operations")(
      test("Long + Float -> Float") {
        assertTrue(comptime(1L + 1.5f) == 2.5f)
      },
      test("Float + Long -> Float") {
        assertTrue(comptime(1.5f + 1L) == 2.5f)
      },
      test("Long * Float -> Float") {
        assertTrue(comptime(2L * 1.5f) == 3.0f)
      },
      test("Long / Float -> Float") {
        assertTrue(comptime(3L / 2.0f) == 1.5f)
      },
      test("Long - Float -> Float") {
        assertTrue(comptime(3L - 1.5f) == 1.5f)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // INT/FLOAT OPERATIONS
    // ─────────────────────────────────────────────────────────────────────────
    suite("Int and Float mixed operations")(
      test("Int / Float -> Float") {
        assertTrue(comptime(3 / 2.0f) == 1.5f)
      },
      test("Float / Int -> Float") {
        assertTrue(comptime(3.0f / 2) == 1.5f)
      },
      test("Int % Float -> Float") {
        assertTrue(comptime(5 % 2.5f) == 0.0f)
      },
      test("Int + Float -> Float") {
        assertTrue(comptime(1 + 0.5f) == 1.5f)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // BYTE/SHORT/CHAR CONVERSIONS
    // Note: Byte.+ and Short.+ are not yet supported as receivers.
    // Char arithmetic is supported.
    // ─────────────────────────────────────────────────────────────────────────
    suite("Byte/Short/Char operations")(
      test("Char - Char -> Int") {
        assertTrue(comptime('b' - 'a') == 1)
      },
      test("Char + Int -> Int") {
        assertTrue(comptime('a' + 1) == 98)
      },
      test("Char - Int -> Int") {
        assertTrue(comptime('z' - 25) == 97)
      },
      test("Byte comparison works") {
        assertTrue(comptime(1.toByte < 2) == true)
      },
      test("Short comparison works") {
        assertTrue(comptime(1.toShort < 2) == true)
      },
      test("Byte == Int") {
        assertTrue(comptime(1.toByte == 1) == true)
      },
      test("Short == Long") {
        assertTrue(comptime(1.toShort == 1L) == true)
      },
      // Byte/Short arithmetic - result type is Int (widening)
      test("Byte + Byte -> Int") {
        assertTrue(comptime(1.toByte + 2.toByte) == 3)
      },
      test("Short + Short -> Int") {
        assertTrue(comptime(1.toShort + 2.toShort) == 3)
      },
      test("Byte * Int -> Int") {
        assertTrue(comptime(2.toByte * 3) == 6)
      },
      test("Short - Long -> Long") {
        assertTrue(comptime(10.toShort - 3L) == 7L)
      },
      test("Byte + Double -> Double") {
        assertTrue(comptime(1.toByte + 1.5) == 2.5)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // CROSS-TYPE EQUALITY
    // ─────────────────────────────────────────────────────────────────────────
    suite("Cross-type equality")(
      test("Int == Long (equal values)") {
        assertTrue(comptime(1 == 1L) == true)
      },
      test("Int == Long (different values)") {
        assertTrue(comptime(1 == 2L) == false)
      },
      test("Int == Double (equal values)") {
        assertTrue(comptime(1 == 1.0) == true)
      },
      test("Int == Double (different values)") {
        assertTrue(comptime(1 == 1.5) == false)
      },
      test("Float == Double (equal values)") {
        assertTrue(comptime(1.0f == 1.0) == true)
      },
      test("Float == Double (precision edge case)") {
        assertTrue(comptime(1.0f == 1.0000001) == false)
      },
      test("Long == Double (equal values)") {
        assertTrue(comptime(100L == 100.0) == true)
      },
      test("Char == Int") {
        assertTrue(comptime('a' == 97) == true)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // BIGINT MIXED OPERATIONS
    // ─────────────────────────────────────────────────────────────────────────
    suite("BigInt mixed operations")(
      test("BigInt + BigInt") {
        assertTrue(comptime(BigInt(1) + BigInt(2)) == BigInt(3))
      },
      test("BigInt * BigInt") {
        assertTrue(comptime(BigInt(3) * BigInt(4)) == BigInt(12))
      },
      test("BigInt comparison") {
        assertTrue(comptime(BigInt(5) > BigInt(3)) == true)
      }
      // Note: BigInt + Int requires implicit conversion (int2bigInt)
      // which may not be supported in comptime
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // BIGDECIMAL MIXED OPERATIONS
    // ─────────────────────────────────────────────────────────────────────────
    suite("BigDecimal mixed operations")(
      test("BigDecimal + BigDecimal") {
        assertTrue(comptime(BigDecimal(1.5) + BigDecimal(2.5)) == BigDecimal(4.0))
      },
      test("BigDecimal * BigDecimal") {
        assertTrue(comptime(BigDecimal(2.0) * BigDecimal(3.0)) == BigDecimal(6.0))
      },
      test("BigDecimal comparison") {
        assertTrue(comptime(BigDecimal(5.0) > BigDecimal(3.0)) == true)
      }
      // Note: BigDecimal + Double requires implicit conversion
      // which may not be supported in comptime
    )
  )
