package comptime

import zio.test.*

object MapExtraSpec extends ZIOSpecDefault:
  val spec =
    suite("MapExtraSpec (comptime)")(
      suite("Map key/value accessors")(
        test("keys") {
          assertTrue(
            comptime(Map("a" -> 1, "b" -> 2).keys.toSet) == Set("a", "b"),
            comptime(Map(1 -> "x", 2 -> "y", 3 -> "z").keys.toSet) == Set(1, 2, 3),
            comptime(Map.empty[String, Int].keys.toList) == List.empty[String]
          )
        },
        test("values") {
          assertTrue(
            comptime(Map("a" -> 1, "b" -> 2).values.toSet) == Set(1, 2),
            comptime(Map(1 -> "x", 2 -> "y").values.toSet) == Set("x", "y"),
            comptime(Map.empty[String, Int].values.toList) == List.empty[Int]
          )
        },
        test("keySet") {
          assertTrue(
            comptime(Map("a" -> 1, "b" -> 2).keySet) == Set("a", "b"),
            comptime(Map(1 -> "x", 2 -> "y", 3 -> "z").keySet) == Set(1, 2, 3),
            comptime(Map.empty[String, Int].keySet) == Set.empty[String]
          )
        }
      ),
      suite("Map transformation operations")(
        test("transform") {
          assertTrue(
            comptime(Map("a" -> 1, "b" -> 2).transform((k, v) => k + v.toString)) == Map("a" -> "a1", "b" -> "b2"),
            comptime(Map(1 -> 10, 2 -> 20).transform((k, v) => k + v)) == Map(1 -> 11, 2 -> 22),
            comptime(Map.empty[String, Int].transform((k, v) => v * 2)) == Map.empty[String, Int]
          )
        }
      ),
      suite("Map filter operations")(
        test("filter on Map") {
          assertTrue(
            comptime(Map("a" -> 1, "b" -> 2, "c" -> 3).filter(_._2 > 1)) == Map("b" -> 2, "c" -> 3),
            comptime(Map(1 -> "x", 2 -> "y").filter(_._1 == 1)) == Map(1 -> "x"),
            // Filter by key - equivalent to filterKeys
            comptime(Map("a" -> 1, "b" -> 2, "c" -> 3).filter { case (k, _) => k != "b" }) == Map("a" -> 1, "c" -> 3)
          )
        },
        test("filterNot on Map") {
          assertTrue(
            comptime(Map("a" -> 1, "b" -> 2, "c" -> 3).filterNot(_._2 > 1)) == Map("a" -> 1),
            comptime(Map(1 -> "x", 2 -> "y").filterNot(_._1 == 1)) == Map(2 -> "y")
          )
        }
      ),
      suite("Map map/flatMap operations")(
        test("map on Map") {
          assertTrue(
            comptime(Map("a" -> 1, "b" -> 2).map { case (k, v) => (k.toUpperCase, v * 10) }) == Map(
              "A" -> 10,
              "B" -> 20
            ),
            comptime(Map(1 -> "x").map { case (k, v) => (k + 1, v + v) }) == Map(2 -> "xx"),
            // Map values only - equivalent to mapValues
            comptime(Map("a" -> 1, "b" -> 2).map { case (k, v) => (k, v * 10) }) == Map("a" -> 10, "b" -> 20)
          )
        },
        test("flatMap on Map") {
          assertTrue(
            comptime(Map("a" -> 1).flatMap { case (k, v) => Map(k -> v, k.toUpperCase -> v * 10) }) == Map(
              "a" -> 1,
              "A" -> 10
            )
          )
        }
      )
    )
