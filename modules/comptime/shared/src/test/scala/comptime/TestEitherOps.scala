package comptime

import zio.test.*

object TestEitherOps extends ZIOSpecDefault:
  val spec =
    suite("Test Either ops")(
      test("Either.left.map") {
        assertTrue(
          comptime(Left[Int, String](1).left.map(_ * 2)) == Left(2)
        )
      },
      test("Either.merge") {
        assertTrue(
          comptime(Right[Int, Int](5).merge) == 5,
          comptime(Left[Int, Int](3).merge) == 3
        )
      }
    )
