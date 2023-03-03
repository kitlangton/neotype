package neotype

import neotype.TestMacros.*
import zio.test.*

object EvalSpec extends ZIOSpecDefault:
  val spec =
    suite("EvalSpec")(
      evalTests.map { case (actual, expected) =>
        test(s"eval($actual) == $expected") {
          assertTrue(actual == expected)
        }
      }
    )

/** Tests various ways of evaluating various expressions
  */
val evalTests =
  List(
    // numeric expressions
    // int
    eval(identity(3) * 2)  -> 6,
    eval(identity(1) + 1)  -> 2,
    eval(identity(10) - 5) -> 5,
    eval(identity(10) / 2) -> 5,
    eval(identity(20) % 3) -> 2,

    // long
    eval(identity(3L) * 2L)  -> 6L,
    eval(identity(1L) + 1L)  -> 2L,
    eval(identity(10L) - 5L) -> 5L,
    eval(identity(10L) / 2L) -> 5L,
    eval(identity(20L) % 3L) -> 2L,

    // double
    eval(identity(3.0) * 2.0)  -> 6.0,
    eval(identity(1.5) + 1.5)  -> 3.0,
    eval(identity(10.0) - 5.5) -> 4.5,
    eval(identity(10.0) / 2.0) -> 5.0,
//    eval(identity(20.0) % 3.0) -> 2.0,

    // string expressions
    eval(identity("Hello, ") + "world!")              -> "Hello, world!",
    eval("Scala is good".toUpperCase)                 -> "SCALA IS GOOD",
    eval("SCALA IS GOOD".toLowerCase)                 -> "scala is good",
    eval("Scala is good".toUpperCase())               -> "SCALA IS GOOD",
    eval("SCALA IS GOOD".toLowerCase())               -> "scala is good",
    eval("myemail@gmail.com".matches(".*@gmail.com")) -> true,

    // boolean expressions
    eval(identity(true) && false) -> false,
    eval(identity(true) || false) -> true,
    eval(!identity(true))         -> false,
    eval(!identity(false))        -> true,
    eval(identity(true) == false) -> false,

    // set expressions
    eval(Set(1, 2, 3))             -> Set(1, 2, 3),
    eval(Set(1, 2, 3) + 4)         -> Set(1, 2, 3, 4),
    eval(Set(1, 2, 3) - 2)         -> Set(1, 3),
    eval(Set(1, 2, 3).contains(2)) -> true,
    eval(Set(1, 2, 3).contains(5)) -> false,
//    eval(Set(1, 2, 3)(5)) -> false,

    // list expressions
    eval(List(1, 2, 3))      -> List(1, 2, 3),
    eval(List(1, 2, 3) :+ 4) -> List(1, 2, 3, 4),
    eval(5 :: List(1, 2, 3)) -> List(5, 1, 2, 3)
//    eval(List(1, 2, 3).head) -> 1
  )
