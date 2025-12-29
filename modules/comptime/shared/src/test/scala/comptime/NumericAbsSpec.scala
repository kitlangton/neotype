package comptime

import zio.test.*

object NumericAbsSpec extends ZIOSpecDefault:
  val spec =
    suite("NumericAbsSpec (comptime)")(
      test("Int abs") {
        assertTrue(
          comptime((-5).abs) == 5,
          comptime(5.abs) == 5
        )
      },
      test("Long abs") {
        assertTrue(
          comptime((-5L).abs) == 5L
        )
      },
      test("Double abs") {
        assertTrue(
          comptime((-5.5).abs) == 5.5
        )
      },
      test("Int sign") {
        assertTrue(
          comptime(5.sign) == 1,
          comptime((-5).sign) == -1,
          comptime(0.sign) == 0
        )
      },
      test("Long signum") {
        assertTrue(
          comptime(5L.signum) == 1,
          comptime((-5L).signum) == -1
        )
      },
      test("Int toHexString") {
        assertTrue(
          comptime(255.toHexString) == "ff",
          comptime(16.toHexString) == "10"
        )
      },
      test("Int toBinaryString") {
        assertTrue(
          comptime(5.toBinaryString) == "101",
          comptime(255.toBinaryString) == "11111111"
        )
      },
      test("Byte abs/sign/signum") {
        assertTrue(
          comptime((-5: Byte).abs) == 5,
          comptime((5: Byte).sign) == 1,
          comptime((-5: Byte).signum) == -1
        )
      },
      test("Short abs/sign/signum") {
        assertTrue(
          comptime((-5: Short).abs) == 5,
          comptime((5: Short).sign) == 1,
          comptime((-5: Short).signum) == -1
        )
      }
    )
