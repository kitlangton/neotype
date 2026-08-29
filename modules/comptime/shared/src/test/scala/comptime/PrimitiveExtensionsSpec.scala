package comptime

import zio.test.*

object PrimitiveExtensionsSpec extends ZIOSpecDefault:
  val spec = suite("PrimitiveExtensionsSpec")(
    test("parameterless extensions on every primitive companion") {
      assertTrue(
        comptime((-3).abs) == 3,
        comptime((-3L).abs) == 3L,
        comptime((-3.5f).abs) == 3.5f,
        comptime((-3.5).abs) == 3.5,
        comptime('a'.toUpper) == 'A',
        comptime((-3).toByte.abs) == 3.toByte,
        comptime((-3).toShort.abs) == 3.toShort
      )
    },
    test("receiver precedes explicit arguments") {
      assertTrue(
        comptime(3.min(5)) == 3,
        comptime(3.min(that = 5)) == 3,
        comptime(3L.max(5L)) == 5L,
        comptime(3.5f.min(5.5f)) == 3.5f,
        comptime(3.5.max(5.5)) == 5.5,
        comptime((2 until 5).toList) == List(2, 3, 4),
        comptime(('b' to 'd').toList) == List('b', 'c', 'd')
      )
    },
    test("extensions compose with ordinary primitive and static methods") {
      assertTrue(
        comptime(List(-3, 1, -2).map(_.abs).sum) == 6,
        comptime(3.min(5).max(4)) == 4,
        comptime('a'.toUpper.toInt) == 65,
        comptime(scala.math.max(3, 5)) == 5,
        comptime(List(3, 5).map(_ + 1).sum) == 10
      )
    }
  )
