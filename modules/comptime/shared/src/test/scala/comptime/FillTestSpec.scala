package comptime

import zio.test.*

object FillTestSpec extends ZIOSpecDefault:
  val spec =
    suite("FillTestSpec")(
      test("List.fill") {
        assertTrue(
          comptime(List.fill(3)("x")) == List("x", "x", "x"),
          comptime(List.fill(0)(42)) == List.empty[Int]
        )
      },
      test("List.tabulate") {
        assertTrue(
          comptime(List.tabulate(3)(i => i * 2)) == List(0, 2, 4),
          comptime(List.tabulate(0)(i => i)) == List.empty[Int]
        )
      },
      test("Vector.fill - preserves Vector type") {
        // Critical: must return Vector, not List (was a ClassCastException bug)
        val result: Vector[Int] = comptime(Vector.fill(3)(1))
        assertTrue(
          result == Vector(1, 1, 1),
          comptime(Vector.fill(0)("x")) == Vector.empty[String]
        )
      },
      test("Vector.tabulate - preserves Vector type") {
        val result: Vector[Int] = comptime(Vector.tabulate(4)(i => i * i))
        assertTrue(
          result == Vector(0, 1, 4, 9),
          comptime(Vector.tabulate(0)(i => i)) == Vector.empty[Int]
        )
      },
      test("Seq.fill") {
        assertTrue(
          comptime(Seq.fill(2)("ab")) == Seq("ab", "ab")
        )
      },
      test("Seq.tabulate") {
        assertTrue(
          comptime(Seq.tabulate(3)(i => i + 1)) == Seq(1, 2, 3)
        )
      },
      test("Iterator.from(start)") {
        assertTrue(
          comptime(Iterator.from(1).take(3).toList) == List(1, 2, 3),
          comptime(Iterator.from(10).take(2).toList) == List(10, 11)
        )
      },
      test("Iterator.from(start, step)") {
        assertTrue(
          comptime(Iterator.from(0, 2).take(4).toList) == List(0, 2, 4, 6),
          comptime(Iterator.from(5, -1).take(3).toList) == List(5, 4, 3)
        )
      },
      test("LazyList.from(start)") {
        assertTrue(
          comptime(LazyList.from(1).take(3).toList) == List(1, 2, 3),
          comptime(LazyList.from(100).take(2).toList) == List(100, 101)
        )
      },
      test("LazyList.from(start, step)") {
        assertTrue(
          comptime(LazyList.from(0, 3).take(4).toList) == List(0, 3, 6, 9),
          comptime(LazyList.from(10, -2).take(3).toList) == List(10, 8, 6)
        )
      }
      // Note: Array.fill requires ClassTag support which is complex to implement
      // in the compile-time evaluator. Skipping for now.
    )
