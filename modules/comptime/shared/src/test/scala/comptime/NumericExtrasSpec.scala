package comptime

import zio.test.*

object NumericExtrasSpec extends ZIOSpecDefault:
  val spec =
    suite("NumericExtrasSpec (comptime)")(
      suite("RichInt max/min")(
        test("max") {
          assertTrue(
            comptime(3.max(5)) == 5,
            comptime(5.max(3)) == 5,
            comptime((-1).max(1)) == 1
          )
        },
        test("min") {
          assertTrue(
            comptime(3.min(5)) == 3,
            comptime(5.min(3)) == 3,
            comptime((-1).min(1)) == -1
          )
        }
      ),
      suite("RichLong max/min")(
        test("max") {
          assertTrue(
            comptime(3L.max(5L)) == 5L,
            comptime(5L.max(3L)) == 5L
          )
        },
        test("min") {
          assertTrue(
            comptime(3L.min(5L)) == 3L,
            comptime(5L.min(3L)) == 3L
          )
        }
      ),
      suite("RichDouble max/min")(
        test("max") {
          assertTrue(
            comptime(3.5.max(5.5)) == 5.5,
            comptime(5.5.max(3.5)) == 5.5
          )
        },
        test("min") {
          assertTrue(
            comptime(3.5.min(5.5)) == 3.5,
            comptime(5.5.min(3.5)) == 3.5
          )
        }
      ),
      suite("Math functions")(
        test("abs") {
          assertTrue(
            comptime(Math.abs(-5)) == 5,
            comptime(Math.abs(-5L)) == 5L,
            comptime(Math.abs(-5.5)) == 5.5,
            comptime(scala.math.abs(-10)) == 10
          )
        },
        test("max/min") {
          assertTrue(
            comptime(Math.max(3, 5)) == 5,
            comptime(Math.min(3, 5)) == 3,
            comptime(Math.max(3.0, 5.0)) == 5.0,
            comptime(Math.min(3.0, 5.0)) == 3.0
          )
        },
        test("sqrt/pow") {
          assertTrue(
            comptime(Math.sqrt(16.0)) == 4.0,
            comptime(Math.pow(2.0, 3.0)) == 8.0
          )
        },
        test("ceil/floor/round") {
          assertTrue(
            comptime(Math.ceil(3.2)) == 4.0,
            comptime(Math.floor(3.8)) == 3.0,
            comptime(Math.round(3.5)) == 4L,
            comptime(Math.round(3.4)) == 3L
          )
        },
        test("trig functions") {
          assertTrue(
            comptime(Math.sin(0.0)) == 0.0,
            comptime(Math.cos(0.0)) == 1.0
          )
        }
      ),
      suite("String strip methods")(
        test("strip") {
          assertTrue(
            comptime("  hello  ".strip) == "hello",
            comptime("\t\nhello\t\n".strip) == "hello"
          )
        },
        test("stripLeading") {
          assertTrue(
            comptime("  hello  ".stripLeading) == "hello  "
          )
        },
        test("stripTrailing") {
          assertTrue(
            comptime("  hello  ".stripTrailing) == "  hello"
          )
        }
      ),
      // List.fill/tabulate are deferred - they dispatch through StrictOptimizedSeqFactory
      // which requires more complex handling
      test("placeholder") {
        assertTrue(true)
      }
    )
