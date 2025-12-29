package comptime

import zio.test.*

object SeqExtraSpec extends ZIOSpecDefault:
  val spec =
    suite("SeqExtraSpec (comptime)")(
      suite("distinctBy")(
        test("distinctBy with key function") {
          assertTrue(
            comptime(List("a", "ab", "abc", "b", "bc").distinctBy(_.length)) == List("a", "ab", "abc"),
            comptime(Vector(1, 2, 3, 4, 5).distinctBy(_ % 2)) == Vector(1, 2),
            comptime(List((1, "a"), (2, "a"), (1, "b")).distinctBy(_._1)) == List((1, "a"), (2, "a"))
          )
        },
        test("distinctBy on empty") {
          assertTrue(
            comptime(List.empty[Int].distinctBy(_ % 2)) == List.empty[Int]
          )
        }
      ),
      suite("transpose")(
        test("transpose nested lists") {
          assertTrue(
            comptime(List(List(1, 2, 3), List(4, 5, 6)).transpose) == List(List(1, 4), List(2, 5), List(3, 6)),
            comptime(Vector(Vector(1, 2), Vector(3, 4), Vector(5, 6)).transpose) == Vector(
              Vector(1, 3, 5),
              Vector(2, 4, 6)
            )
          )
        },
        test("transpose single row") {
          assertTrue(
            comptime(List(List(1, 2, 3)).transpose) == List(List(1), List(2), List(3))
          )
        },
        test("transpose single column") {
          assertTrue(
            comptime(List(List(1), List(2), List(3)).transpose) == List(List(1, 2, 3))
          )
        },
        test("transpose empty") {
          assertTrue(
            comptime(List.empty[List[Int]].transpose) == List.empty[List[Int]]
          )
        }
      ),
      suite("corresponds")(
        test("corresponds with equal lengths - matching") {
          assertTrue(
            comptime(List(1, 2, 3).corresponds(List(2, 4, 6))(_ * 2 == _)) == true,
            comptime(Vector("a", "bb", "ccc").corresponds(Vector(1, 2, 3))(_.length == _)) == true
          )
        },
        test("corresponds with equal lengths - not matching") {
          assertTrue(
            comptime(List(1, 2, 3).corresponds(List(2, 4, 5))(_ * 2 == _)) == false
          )
        },
        test("corresponds with different lengths") {
          assertTrue(
            comptime(List(1, 2, 3).corresponds(List(2, 4))(_ * 2 == _)) == false,
            comptime(List(1, 2).corresponds(List(2, 4, 6))(_ * 2 == _)) == false
          )
        },
        test("corresponds empty") {
          assertTrue(
            comptime(List.empty[Int].corresponds(List.empty[Int])(_ == _)) == true,
            comptime(List.empty[Int].corresponds(List(1))(_ == _)) == false
          )
        }
      ),
      suite("sameElements")(
        test("sameElements - matching") {
          assertTrue(
            comptime(List(1, 2, 3).sameElements(List(1, 2, 3))) == true,
            comptime(Vector(1, 2, 3).sameElements(Vector(1, 2, 3))) == true,
            comptime(List("a", "b").sameElements(List("a", "b"))) == true
          )
        },
        test("sameElements - not matching (different values)") {
          assertTrue(
            comptime(List(1, 2, 3).sameElements(List(1, 2, 4))) == false
          )
        },
        test("sameElements - not matching (different lengths)") {
          assertTrue(
            comptime(List(1, 2, 3).sameElements(List(1, 2))) == false,
            comptime(List(1, 2).sameElements(List(1, 2, 3))) == false
          )
        },
        test("sameElements - cross collection types") {
          assertTrue(
            comptime(List(1, 2, 3).sameElements(Vector(1, 2, 3))) == true
          )
        },
        test("sameElements - empty") {
          assertTrue(
            comptime(List.empty[Int].sameElements(List.empty[Int])) == true,
            comptime(List.empty[Int].sameElements(List(1))) == false
          )
        }
      )
    )
