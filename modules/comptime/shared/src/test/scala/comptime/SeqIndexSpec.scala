package comptime

import zio.test.*

object SeqIndexSpec extends ZIOSpecDefault:
  val spec =
    suite("SeqIndexSpec (comptime)")(
      suite("indexOf operations")(
        test("indexOf(elem)") {
          assertTrue(
            comptime(List(1, 2, 3, 2, 1).indexOf(2)) == 1,
            comptime(List(1, 2, 3).indexOf(5)) == -1,
            comptime(Vector("a", "b", "c").indexOf("b")) == 1,
            comptime(List.empty[Int].indexOf(1)) == -1
          )
        },
        test("indexOf(elem, from)") {
          assertTrue(
            comptime(List(1, 2, 3, 2, 1).indexOf(2, 2)) == 3,
            comptime(List(1, 2, 3, 2, 1).indexOf(2, 0)) == 1,
            comptime(Vector("a", "b", "c", "b").indexOf("b", 2)) == 3,
            comptime(List(1, 2, 3).indexOf(2, 5)) == -1
          )
        },
        test("lastIndexOf(elem)") {
          assertTrue(
            comptime(List(1, 2, 3, 2, 1).lastIndexOf(2)) == 3,
            comptime(List(1, 2, 3).lastIndexOf(5)) == -1,
            comptime(Vector("a", "b", "c", "b").lastIndexOf("b")) == 3,
            comptime(List.empty[Int].lastIndexOf(1)) == -1
          )
        },
        test("lastIndexOf(elem, end)") {
          assertTrue(
            comptime(List(1, 2, 3, 2, 1).lastIndexOf(2, 2)) == 1,
            comptime(List(1, 2, 3, 2, 1).lastIndexOf(2, 4)) == 3,
            comptime(Vector("a", "b", "c", "b").lastIndexOf("b", 1)) == 1,
            comptime(List(1, 2, 3, 2).lastIndexOf(2, 0)) == -1
          )
        }
      ),
      suite("indexWhere operations")(
        test("indexWhere(p)") {
          assertTrue(
            comptime(List(1, 2, 3, 4).indexWhere(_ > 2)) == 2,
            comptime(List(1, 2, 3).indexWhere(_ > 10)) == -1,
            comptime(Vector(1, 2, 3, 4).indexWhere(_ % 2 == 0)) == 1,
            comptime(List.empty[Int].indexWhere(_ > 0)) == -1
          )
        },
        test("indexWhere(p, from)") {
          assertTrue(
            comptime(List(1, 2, 3, 4, 5).indexWhere(_ > 2, 3)) == 3,
            comptime(List(1, 2, 3, 4).indexWhere(_ > 2, 0)) == 2,
            comptime(Vector(1, 2, 3, 4).indexWhere(_ % 2 == 0, 2)) == 3,
            comptime(List(1, 2, 3).indexWhere(_ > 2, 5)) == -1
          )
        },
        test("lastIndexWhere(p)") {
          assertTrue(
            comptime(List(1, 2, 3, 4, 2).lastIndexWhere(_ < 3)) == 4,
            comptime(List(5, 6, 7).lastIndexWhere(_ < 3)) == -1,
            comptime(Vector(1, 2, 3, 2, 1).lastIndexWhere(_ == 2)) == 3,
            comptime(List.empty[Int].lastIndexWhere(_ > 0)) == -1
          )
        },
        test("lastIndexWhere(p, end)") {
          assertTrue(
            comptime(List(1, 2, 3, 4, 2).lastIndexWhere(_ < 3, 2)) == 1,
            comptime(List(1, 2, 3, 4, 2).lastIndexWhere(_ < 3, 4)) == 4,
            comptime(Vector(1, 2, 3, 2, 1).lastIndexWhere(_ == 2, 2)) == 1,
            comptime(List(1, 2, 3).lastIndexWhere(_ > 0, 0)) == 0
          )
        }
      ),
      suite("patch operations")(
        test("patch(from, other, replaced)") {
          assertTrue(
            comptime(List(1, 2, 3, 4, 5).patch(1, List(10, 20), 2)) == List(1, 10, 20, 4, 5),
            comptime(List(1, 2, 3).patch(0, List(9), 1)) == List(9, 2, 3),
            comptime(Vector(1, 2, 3).patch(3, Vector(4, 5), 0)) == Vector(1, 2, 3, 4, 5),
            comptime(List(1, 2, 3).patch(1, List(), 1)) == List(1, 3),
            comptime(List(1, 2, 3).patch(0, List(4, 5, 6), 3)) == List(4, 5, 6)
          )
        }
      ),
      suite("padTo operations")(
        test("padTo(len, elem)") {
          assertTrue(
            comptime(List(1, 2, 3).padTo(5, 0)) == List(1, 2, 3, 0, 0),
            comptime(List(1, 2, 3).padTo(3, 0)) == List(1, 2, 3),
            comptime(List(1, 2, 3).padTo(2, 0)) == List(1, 2, 3),
            comptime(Vector("a", "b").padTo(4, "x")) == Vector("a", "b", "x", "x"),
            comptime(List.empty[Int].padTo(3, 1)) == List(1, 1, 1)
          )
        }
      )
    )
