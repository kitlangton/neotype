package comptime

import zio.test.*

object SetSpec extends ZIOSpecDefault:
  val spec =
    suite("SetSpec (comptime)")(
      suite("Set basics")(
        test("size, isEmpty, nonEmpty") {
          assertTrue(
            comptime(Set(1, 2, 3).size) == 3,
            comptime(Set.empty[Int].isEmpty) == true,
            comptime(Set(1).nonEmpty) == true
          )
        },
        test("contains") {
          assertTrue(
            comptime(Set(1, 2, 3).contains(2)) == true,
            comptime(Set(1, 2, 3).contains(5)) == false
          )
        }
      ),
      suite("Set operations")(
        test("union / ++") {
          assertTrue(
            comptime(Set(1, 2) ++ Set(3, 4)) == Set(1, 2, 3, 4),
            comptime(Set(1, 2).union(Set(2, 3))) == Set(1, 2, 3)
          )
        },
        test("| (union)") {
          assertTrue(
            comptime(Set(1, 2) | Set(2, 3)) == Set(1, 2, 3)
          )
        },
        test("intersect / &") {
          assertTrue(
            comptime(Set(1, 2, 3).intersect(Set(2, 3, 4))) == Set(2, 3),
            comptime(Set(1, 2, 3) & Set(2, 3, 4)) == Set(2, 3)
          )
        },
        test("diff / --") {
          assertTrue(
            comptime(Set(1, 2, 3).diff(Set(2))) == Set(1, 3),
            comptime(Set(1, 2, 3) -- Set(2, 3)) == Set(1)
          )
        },
        test("subsetOf") {
          assertTrue(
            comptime(Set(1, 2).subsetOf(Set(1, 2, 3))) == true,
            comptime(Set(1, 4).subsetOf(Set(1, 2, 3))) == false
          )
        }
      ),
      suite("Element operations")(
        test("+ (incl)") {
          assertTrue(
            comptime(Set(1, 2) + 3) == Set(1, 2, 3),
            comptime(Set(1, 2).incl(3)) == Set(1, 2, 3)
          )
        },
        test("- (excl)") {
          assertTrue(
            comptime(Set(1, 2, 3) - 2) == Set(1, 3),
            comptime(Set(1, 2, 3).excl(2)) == Set(1, 3)
          )
        }
      ),
      suite("HOF operations")(
        test("map") {
          assertTrue(
            comptime(Set(1, 2, 3).map(_ * 2)) == Set(2, 4, 6)
          )
        },
        test("filter") {
          assertTrue(
            comptime(Set(1, 2, 3, 4).filter(_ % 2 == 0)) == Set(2, 4)
          )
        },
        test("filterNot") {
          assertTrue(
            comptime(Set(1, 2, 3, 4).filterNot(_ % 2 == 0)) == Set(1, 3)
          )
        },
        test("flatMap") {
          assertTrue(
            comptime(Set(1, 2).flatMap(x => Set(x, x * 10))) == Set(1, 10, 2, 20)
          )
        }
      )
    )
