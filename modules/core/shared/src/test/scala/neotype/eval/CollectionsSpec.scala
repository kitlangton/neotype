package neotype.eval

import neotype.TestMacros.*
import zio.test.*

object CollectionsSpec extends ZIOSpecDefault:
  val spec =
    suite("CollectionsSpec")(
      evalTests.map { case (actual, expected) =>
        test(s"eval($actual) == $expected") {
          assertTrue(actual == expected)
        }
      }
    )

  inline def wrap[A](a: A): A = a

  lazy val evalTests = List(
    // List specific operations
    eval(List(1, 2, 3)(2))   -> 3,
    eval(List(1, 2, 3))      -> List(1, 2, 3),
    eval(List(1, 2, 3) :+ 4) -> List(1, 2, 3, 4),
    eval(5 :: List(1, 2, 3)) -> List(5, 1, 2, 3),
    eval(List(1, 2, 3).head) -> 1,

    // List transformations
    eval(List(1, 2, 3).filter(_ > 1)) -> List(2, 3),
    eval(List(1, 2, 3).map(_ * 2))    -> List(2, 4, 6),

    // List state checks
    eval(List(1, 2, 3).isEmpty)  -> false,
    eval(List(1, 2, 3).nonEmpty) -> true,

    // Set operations
    eval(Set(1, 2, 3))             -> Set(1, 2, 3),
    eval(Set(1, 2, 3) + 4)         -> Set(1, 2, 3, 4),
    eval(Set(1, 2, 3).contains(2)) -> true,
    eval(Set(1, 2, 3).contains(4)) -> false,

    // Vector operations
    eval(Vector(1, 2, 3)) -> Vector(1, 2, 3)
    // eval(Vector(1, 2, 3) :+ 4)          -> Vector(1, 2, 3, 4),
    // eval(Vector(1, 2, 3).updated(1, 5)) -> Vector(1, 5, 3)
  )
