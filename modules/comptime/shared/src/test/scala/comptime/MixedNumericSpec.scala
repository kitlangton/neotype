package comptime

import zio.test.*

// ═══════════════════════════════════════════════════════════════════════════
// MIXED NUMERIC OPERATIONS - Cross-type comparisons and arithmetic
// ═══════════════════════════════════════════════════════════════════════════
//
// Scala allows comparing/operating on different numeric types (Int vs Double,
// Long vs Float, etc.) with automatic widening. Comptime must support this.
//
object MixedNumericSpec extends ZIOSpecDefault:
  val spec = suite("MixedNumericSpec")(
    // ─────────────────────────────────────────────────────────────────────────
    // DOUBLE vs INT comparisons
    // ─────────────────────────────────────────────────────────────────────────
    suite("Double vs Int comparisons")(
      test("Double < Int") {
        assertTrue(comptime(1.5 < 2) == true)
      },
      test("Double <= Int") {
        assertTrue(comptime(1.5 <= 2) == true)
      },
      test("Double <= Int (equal after conversion)") {
        assertTrue(comptime(2.0 <= 2) == true)
      },
      test("Double > Int") {
        assertTrue(comptime(2.5 > 2) == true)
      },
      test("Double >= Int") {
        assertTrue(comptime(2.5 >= 2) == true)
      },
      test("Double == Int") {
        assertTrue(comptime(2.0 == 2) == true)
      },
      test("Double != Int") {
        assertTrue(comptime(2.5 != 2) == true)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // DOUBLE vs LONG comparisons
    // ─────────────────────────────────────────────────────────────────────────
    suite("Double vs Long comparisons")(
      test("Double < Long") {
        assertTrue(comptime(1.5 < 2L) == true)
      },
      test("Double <= Long") {
        assertTrue(comptime(2.0 <= 2L) == true)
      },
      test("Double > Long") {
        assertTrue(comptime(2.5 > 2L) == true)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // FLOAT vs INT comparisons
    // ─────────────────────────────────────────────────────────────────────────
    suite("Float vs Int comparisons")(
      test("Float < Int") {
        assertTrue(comptime(1.5f < 2) == true)
      },
      test("Float <= Int") {
        assertTrue(comptime(1.5f <= 2) == true)
      },
      test("Float > Int") {
        assertTrue(comptime(2.5f > 2) == true)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // LONG vs INT comparisons
    // ─────────────────────────────────────────────────────────────────────────
    suite("Long vs Int comparisons")(
      test("Long < Int") {
        assertTrue(comptime(1L < 2) == true)
      },
      test("Long <= Int") {
        assertTrue(comptime(2L <= 2) == true)
      },
      test("Long > Int") {
        assertTrue(comptime(3L > 2) == true)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // MIXED ARITHMETIC (Double +/-/*// Int)
    // ─────────────────────────────────────────────────────────────────────────
    suite("Double and Int arithmetic")(
      test("Double + Int") {
        assertTrue(comptime(1.5 + 2) == 3.5)
      },
      test("Double - Int") {
        assertTrue(comptime(3.5 - 2) == 1.5)
      },
      test("Double * Int") {
        assertTrue(comptime(1.5 * 2) == 3.0)
      },
      test("Double / Int") {
        assertTrue(comptime(3.0 / 2) == 1.5)
      },
      test("Int + Double") {
        assertTrue(comptime(2 + 1.5) == 3.5)
      },
      test("Int - Double") {
        assertTrue(comptime(3 - 1.5) == 1.5)
      },
      test("Int * Double") {
        assertTrue(comptime(2 * 1.5) == 3.0)
      },
      test("Int / Double") {
        assertTrue(comptime(3 / 2.0) == 1.5)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // MIXED ARITHMETIC (Float + Int)
    // ─────────────────────────────────────────────────────────────────────────
    suite("Float and Int arithmetic")(
      test("Float + Int") {
        assertTrue(comptime(1.5f + 2) == 3.5f)
      },
      test("Int + Float") {
        assertTrue(comptime(2 + 1.5f) == 3.5f)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // MIXED ARITHMETIC (Long + Int)
    // ─────────────────────────────────────────────────────────────────────────
    suite("Long and Int arithmetic")(
      test("Long + Int") {
        assertTrue(comptime(1L + 2) == 3L)
      },
      test("Int + Long") {
        assertTrue(comptime(2 + 1L) == 3L)
      },
      test("Long * Int") {
        assertTrue(comptime(3L * 2) == 6L)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // CASE CLASS FIELD ACCESS with mixed comparisons
    // ─────────────────────────────────────────────────────────────────────────
    suite("Case class Double fields vs Int literals")(
      test("case class Double field <= Int literal") {
        case class Box(width: Double, height: Double)
        assertTrue(comptime {
          val box = Box(5.0, 3.0)
          box.width <= 10
        } == true)
      },
      test("case class Double field > Int literal (false case)") {
        case class Box(width: Double, height: Double)
        assertTrue(comptime {
          val box = Box(5.0, 3.0)
          box.width > 10
        } == false)
      },
      test("case class Double field <= Int zero") {
        case class Box(width: Double, height: Double)
        assertTrue(comptime {
          val box = Box(5.0, 3.0)
          box.width <= 0
        } == false)
      },
      test("case class Double field > Int zero") {
        case class Box(width: Double, height: Double)
        assertTrue(comptime {
          val box = Box(5.0, 3.0)
          box.width > 0
        } == true)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // CRITICAL EDGE CASES - Int/Long/Float receiver with wider RHS
    // These were buggy before: narrowing RHS instead of widening to common type
    // ─────────────────────────────────────────────────────────────────────────
    suite("Int receiver with Double RHS - must widen to Double")(
      test("1 < 1.5 should be true (not truncate to 1 < 1)") {
        assertTrue(comptime(1 < 1.5) == true)
      },
      test("1 == 1.1 should be false (not truncate to 1 == 1)") {
        assertTrue(comptime(1 == 1.1) == false)
      },
      test("2 > 1.9 should be true") {
        assertTrue(comptime(2 > 1.9) == true)
      },
      test("1 >= 1.0 should be true") {
        assertTrue(comptime(1 >= 1.0) == true)
      }
    ),
    suite("Long receiver with Double RHS - must widen to Double")(
      test("1L < 1.5 should be true") {
        assertTrue(comptime(1L < 1.5) == true)
      },
      test("1L == 1.1 should be false") {
        assertTrue(comptime(1L == 1.1) == false)
      },
      test("2L > 1.9 should be true") {
        assertTrue(comptime(2L > 1.9) == true)
      }
    ),
    suite("Long receiver with Float RHS - must widen to Float")(
      test("1L < 1.5f should be true") {
        assertTrue(comptime(1L < 1.5f) == true)
      },
      test("1L == 1.1f should be false") {
        assertTrue(comptime(1L == 1.1f) == false)
      }
    ),
    suite("Float receiver with Double RHS - must widen to Double")(
      test("1.0f < 1.5 should be true") {
        assertTrue(comptime(1.0f < 1.5) == true)
      },
      test("1.5f == 1.5 should be true") {
        assertTrue(comptime(1.5f == 1.5) == true)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // ARITHMETIC TYPE COERCION - Result type must be widest operand
    // ─────────────────────────────────────────────────────────────────────────
    suite("Arithmetic result types")(
      test("Long + Double -> Double") {
        assertTrue(comptime(1L + 1.5) == 2.5)
      },
      test("Float + Double -> Double") {
        assertTrue(comptime(1.5f + 1.5) == 3.0)
      },
      test("Int + Double -> Double") {
        assertTrue(comptime(1 + 1.5) == 2.5)
      },
      test("Long * Double -> Double") {
        assertTrue(comptime(2L * 1.5) == 3.0)
      },
      test("Float * Double -> Double") {
        assertTrue(comptime(2.0f * 1.5) == 3.0)
      }
    ),

    // ─────────────────────────────────────────────────────────────────────────
    // BYTE/SHORT with wider types
    // ─────────────────────────────────────────────────────────────────────────
    suite("Byte/Short with wider types")(
      test("Byte < Double") {
        assertTrue(comptime(1.toByte < 1.5) == true)
      },
      test("Short < Double") {
        assertTrue(comptime(1.toShort < 1.5) == true)
      },
      test("Byte == Int") {
        assertTrue(comptime(1.toByte == 1) == true)
      },
      test("Short > Long") {
        assertTrue(comptime(10.toShort > 5L) == true)
      }
    )
  )
