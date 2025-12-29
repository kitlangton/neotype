package comptime

import zio.test.*

object TupleExtraSpec extends ZIOSpecDefault:
  val spec =
    suite("TupleExtraSpec (comptime)")(
      suite("Tuple2 operations")(
        test("swap") {
          assertTrue(
            comptime((1, "a").swap) == ("a", 1),
            comptime(("hello", 42).swap) == (42, "hello"),
            comptime((true, false).swap) == (false, true)
          )
        }
      )
    )
