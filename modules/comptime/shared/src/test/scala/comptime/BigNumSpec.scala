package comptime

import zio.test.*

object BigNumSpec extends ZIOSpecDefault:
  val spec =
    suite("BigNumSpec")(
      suite("BigInt")(
        test("arithmetic ops") {
          assertTrue(
            comptime(BigInt(1) + BigInt(2)) == BigInt(3),
            comptime(BigInt(5) - BigInt(2)) == BigInt(3),
            comptime(BigInt(4) * BigInt(3)) == BigInt(12),
            comptime(BigInt(8) / BigInt(2)) == BigInt(4),
            comptime(BigInt(8) % BigInt(3)) == BigInt(2),
            comptime(-BigInt(3)) == BigInt(-3)
          )
        },
        test("comparison ops") {
          assertTrue(
            comptime(BigInt(4) == BigInt(4)),
            !comptime(BigInt(4) == BigInt(5)),
            comptime(BigInt(4) != BigInt(5)),
            comptime(BigInt(2) < BigInt(3)),
            comptime(BigInt(2) <= BigInt(2)),
            comptime(BigInt(3) > BigInt(2)),
            comptime(BigInt(3) >= BigInt(3))
          )
        },
        test("conversion ops") {
          assertTrue(
            comptime(BigInt(42).toInt) == 42,
            comptime(BigInt(42).toLong) == 42L,
            comptime(BigInt(42).toDouble) == 42.0,
            comptime(BigInt(42).toString) == "42"
          )
        },
        test("specific ops") {
          assertTrue(
            comptime(BigInt(10).mod(BigInt(3))) == BigInt(1),
            comptime(BigInt(2).pow(10)) == BigInt(1024),
            comptime(BigInt(-5).abs) == BigInt(5),
            comptime(BigInt(5).abs) == BigInt(5),
            comptime(BigInt(-5).signum) == -1,
            comptime(BigInt(0).signum) == 0,
            comptime(BigInt(5).signum) == 1,
            comptime(BigInt(255).bitLength) == 8,
            comptime(BigInt(256).bitLength) == 9
          )
        },
        test("compare method") {
          assertTrue(
            comptime(BigInt(3).compare(BigInt(5))) < 0,
            comptime(BigInt(5).compare(BigInt(3))) > 0,
            comptime(BigInt(3).compare(BigInt(3))) == 0
          )
        },
        test("min/max") {
          assertTrue(
            comptime(BigInt(3).min(BigInt(5))) == BigInt(3),
            comptime(BigInt(5).min(BigInt(3))) == BigInt(3),
            comptime(BigInt(3).max(BigInt(5))) == BigInt(5),
            comptime(BigInt(5).max(BigInt(3))) == BigInt(5)
          )
        }
      ),
      suite("BigDecimal")(
        test("arithmetic ops") {
          assertTrue(
            comptime(BigDecimal(1) + BigDecimal(2)) == BigDecimal(3),
            comptime(BigDecimal(5) - BigDecimal(2)) == BigDecimal(3),
            comptime(BigDecimal(4) * BigDecimal(3)) == BigDecimal(12),
            comptime(BigDecimal(8) / BigDecimal(2)) == BigDecimal(4),
            comptime(BigDecimal(8.5) % BigDecimal(3)) == BigDecimal(2.5),
            comptime(-BigDecimal(3)) == BigDecimal(-3)
          )
        },
        test("comparison ops") {
          assertTrue(
            comptime(BigDecimal(4) == BigDecimal(4)),
            !comptime(BigDecimal(4) == BigDecimal(5)),
            comptime(BigDecimal(4) != BigDecimal(5)),
            comptime(BigDecimal(2) < BigDecimal(3)),
            comptime(BigDecimal(2) <= BigDecimal(2)),
            comptime(BigDecimal(3) > BigDecimal(2)),
            comptime(BigDecimal(3) >= BigDecimal(3))
          )
        },
        test("conversion ops") {
          assertTrue(
            comptime(BigDecimal(42).toInt) == 42,
            comptime(BigDecimal(42).toLong) == 42L,
            comptime(BigDecimal(42.5).toDouble) == 42.5,
            comptime(BigDecimal(42.5).toString) == "42.5"
          )
        },
        test("specific ops") {
          assertTrue(
            comptime(BigDecimal(3.14159).precision) == 6,
            comptime(BigDecimal(3.14).scale) == 2,
            comptime(BigDecimal(-5.5).abs) == BigDecimal(5.5),
            comptime(BigDecimal(5.5).abs) == BigDecimal(5.5),
            comptime(BigDecimal(-5.5).signum) == -1,
            comptime(BigDecimal(0).signum) == 0,
            comptime(BigDecimal(5.5).signum) == 1
          )
        },
        test("compare method") {
          assertTrue(
            comptime(BigDecimal(3.5).compare(BigDecimal(5.5))) < 0,
            comptime(BigDecimal(5.5).compare(BigDecimal(3.5))) > 0,
            comptime(BigDecimal(3.5).compare(BigDecimal(3.5))) == 0
          )
        },
        test("min/max") {
          assertTrue(
            comptime(BigDecimal(3.5).min(BigDecimal(5.5))) == BigDecimal(3.5),
            comptime(BigDecimal(5.5).min(BigDecimal(3.5))) == BigDecimal(3.5),
            comptime(BigDecimal(3.5).max(BigDecimal(5.5))) == BigDecimal(5.5),
            comptime(BigDecimal(5.5).max(BigDecimal(3.5))) == BigDecimal(5.5)
          )
        }
      ),
      suite("BigInt constructors")(
        test("from Int/Long/String") {
          assertTrue(
            comptime(BigInt(42)) == BigInt(42),
            comptime(BigInt(42L)) == BigInt(42),
            comptime(BigInt("42")) == BigInt(42)
          )
        }
      ),
      suite("BigDecimal constructors")(
        test("from Int/Long/Double/String") {
          assertTrue(
            comptime(BigDecimal(42)) == BigDecimal(42),
            comptime(BigDecimal(42L)) == BigDecimal(42),
            comptime(BigDecimal(42.5)) == BigDecimal(42.5),
            comptime(BigDecimal("42.5")) == BigDecimal(42.5)
          )
        }
      )
    )
