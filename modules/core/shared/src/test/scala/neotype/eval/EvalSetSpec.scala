package neotype.eval

import neotype.TestMacros.*
import zio.test.*

object EvalSetSpec extends ZIOSpecDefault:
  val spec =
    suite("EvalSetSpec")(
      evalTests.map { case (actual, expected) =>
        test(s"eval($actual) == $expected") {
          assertTrue(actual == expected)
        }
      }
    )

  private lazy val evalTests =
    List(
      // set expressions
      eval(Set(1, 2, 3))                        -> Set(1, 2, 3),
      eval(Set(1, 2, 3) + 4)                    -> Set(1, 2, 3, 4),
      eval(Set(1, 2, 3) - 2)                    -> Set(1, 3),
      eval(Set(1, 2, 3).contains(2))            -> true,
      eval(Set(1, 2, 3).contains(5))            -> false,
      eval(Set(1, 2, 3)(5))                     -> false,
      eval(Set(1, 2, 3) intersect Set(2, 3, 4)) -> Set(2, 3),
      eval(Set(1, 2, 3) ++ Set(2, 3, 4))        -> Set(1, 2, 3, 4),
      eval(Set(1, 2, 3) -- Set(2, 3, 4))        -> Set(1),
      eval(Set(1, 2, 3).filter(_ > 1))          -> Set(2, 3),
      eval(Set(1, 2, 3).map(_ * 2))             -> Set(2, 4, 6)
    )
