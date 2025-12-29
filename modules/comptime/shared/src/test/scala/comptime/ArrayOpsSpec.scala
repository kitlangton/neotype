package comptime

import zio.test.*

object ArrayOpsSpec extends ZIOSpecDefault:
  val spec =
    suite("ArrayOpsSpec")(
      test("Array.toList") {
        assertTrue(
          comptime(Array(1, 2, 3).toList) == List(1, 2, 3)
        )
      },
      test("Array.size and length") {
        assertTrue(
          comptime(Array(1, 2, 3).size) == 3,
          comptime(Array(1, 2, 3).length) == 3
        )
      },
      test("Array.head and tail") {
        assertTrue(
          comptime(Array(1, 2, 3).head) == 1,
          comptime(Array(1, 2, 3).tail.toList) == List(2, 3)
        )
      },
      test("Array.take and drop") {
        assertTrue(
          comptime(Array(1, 2, 3, 4, 5).take(3).toList) == List(1, 2, 3),
          comptime(Array(1, 2, 3, 4, 5).drop(2).toList) == List(3, 4, 5)
        )
      },
      test("Array.nonEmpty") {
        assertTrue(
          comptime(Array(1, 2, 3).nonEmpty) == true
        )
      },
      test("Array.exists and forall") {
        assertTrue(
          comptime(Array(1, 2, 3).exists(_ > 2)) == true,
          comptime(Array(1, 2, 3).forall(_ > 0)) == true
        )
      },
      test("Array.filter") {
        assertTrue(
          comptime(Array(1, 2, 3, 4).filter(_ % 2 == 0).toList) == List(2, 4)
        )
      },
      test("Array.foldLeft") {
        assertTrue(
          comptime(Array(1, 2, 3).foldLeft(0)(_ + _)) == 6
        )
      }
      // Note: Array.map, Array.flatMap require ArrayOps.map/flatMap rules which need ClassTag support
      // The workaround is to use filter/foldLeft/etc. which work through Seq conversion
    )
