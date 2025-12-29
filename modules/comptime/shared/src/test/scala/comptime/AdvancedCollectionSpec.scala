package comptime

import zio.test.*

object AdvancedCollectionSpec extends ZIOSpecDefault:
  val spec =
    suite("AdvancedCollectionSpec (comptime)")(
      suite("partitionMap")(
        test("partitions into Left and Right") {
          assertTrue(
            comptime(List(1, 2, 3, 4).partitionMap(x => if x % 2 == 0 then Right(x) else Left(x))) == (
              List(1, 3),
              List(2, 4)
            ),
            comptime(Vector(1, 2, 3).partitionMap(x => if x > 1 then Right(x * 10) else Left(x.toString))) == (
              Vector("1"),
              Vector(20, 30)
            )
          )
        },
        test("handles all Left") {
          assertTrue(
            comptime(List(1, 2, 3).partitionMap(x => Left(x))) == (List(1, 2, 3), List())
          )
        },
        test("handles all Right") {
          assertTrue(
            comptime(List(1, 2, 3).partitionMap(x => Right(x))) == (List(), List(1, 2, 3))
          )
        },
        test("handles empty collection") {
          assertTrue(
            comptime(List.empty[Int].partitionMap(x => Right(x))) == (List(), List())
          )
        }
      ),
      suite("groupMap")(
        test("groups by key and maps values") {
          assertTrue(
            comptime(List("a", "bb", "ccc", "dd").groupMap(_.length)(_.toUpperCase)) == Map(
              1 -> List("A"),
              2 -> List("BB", "DD"),
              3 -> List("CCC")
            ),
            comptime(Vector(1, 2, 3, 4, 5).groupMap(_ % 2)(_ * 10)) == Map(0 -> Vector(20, 40), 1 -> Vector(10, 30, 50))
          )
        },
        test("handles single group") {
          assertTrue(
            comptime(List(1, 2, 3).groupMap(_ => "same")(_ * 2)) == Map("same" -> List(2, 4, 6))
          )
        },
        test("handles empty collection") {
          assertTrue(
            comptime(List.empty[Int].groupMap(_ % 2)(identity)) == Map.empty[Int, List[Int]]
          )
        }
      ),
      suite("groupMapReduce")(
        test("groups, maps and reduces in one pass") {
          assertTrue(
            comptime(List("a", "bb", "ccc", "dd").groupMapReduce(_.length)(_.length)(_ + _)) == Map(
              1 -> 1,
              2 -> 4,
              3 -> 3
            ),
            comptime(Vector(1, 2, 3, 4, 5).groupMapReduce(_ % 2)(identity)(_ + _)) == Map(0 -> 6, 1 -> 9)
          )
        },
        test("handles single element groups") {
          assertTrue(
            comptime(List(1, 2, 3).groupMapReduce(identity)(_ * 10)(_ + _)) == Map(1 -> 10, 2 -> 20, 3 -> 30)
          )
        },
        test("handles empty collection") {
          assertTrue(
            comptime(List.empty[Int].groupMapReduce(_ % 2)(identity)(_ + _)) == Map.empty[Int, Int]
          )
        }
      ),
      suite("tapEach")(
        test("returns original collection after side effect") {
          // Note: we can't test the side effect directly, but we can verify the return value
          assertTrue(
            comptime(List(1, 2, 3).tapEach(_ => ())) == List(1, 2, 3),
            comptime(Vector("a", "b").tapEach(_ => ())) == Vector("a", "b")
          )
        },
        test("handles empty collection") {
          assertTrue(
            comptime(List.empty[Int].tapEach(_ => ())) == List.empty[Int]
          )
        }
      ),
      suite("scanLeft")(
        test("produces all intermediate results") {
          assertTrue(
            comptime(List(1, 2, 3).scanLeft(0)(_ + _)) == List(0, 1, 3, 6),
            comptime(Vector(1, 2, 3, 4).scanLeft(1)(_ * _)) == Vector(1, 1, 2, 6, 24)
          )
        },
        test("handles string concatenation") {
          assertTrue(
            comptime(List("a", "b", "c").scanLeft("")(_ + _)) == List("", "a", "ab", "abc")
          )
        },
        test("handles empty collection") {
          assertTrue(
            comptime(List.empty[Int].scanLeft(0)(_ + _)) == List(0)
          )
        }
      ),
      suite("scanRight")(
        test("produces all intermediate results from right") {
          assertTrue(
            comptime(List(1, 2, 3).scanRight(0)(_ + _)) == List(6, 5, 3, 0),
            comptime(Vector(1, 2, 3, 4).scanRight(1)(_ * _)) == Vector(24, 24, 12, 4, 1)
          )
        },
        test("handles string concatenation") {
          assertTrue(
            comptime(List("a", "b", "c").scanRight("")(_ + _)) == List("abc", "bc", "c", "")
          )
        },
        test("handles empty collection") {
          assertTrue(
            comptime(List.empty[Int].scanRight(0)(_ + _)) == List(0)
          )
        },
        test("non-associative operation shows right-to-left order") {
          // scanRight(z)(op) should produce: [op(1, op(2, op(3, z))), op(2, op(3, z)), op(3, z), z]
          // For subtraction: 1 - (2 - (3 - 0)) = 1 - (2 - 3) = 1 - (-1) = 2
          assertTrue(
            comptime(List(1, 2, 3).scanRight(0)(_ - _)) == List(2, -1, 3, 0)
          )
        }
      )
    )
