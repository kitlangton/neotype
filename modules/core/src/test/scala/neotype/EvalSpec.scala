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

inline def wrap[A](a: A): A = a

/** Tests various ways of evaluating various expressions
  */
val evalTests =
  List(
    // numeric expressions
    // int
    eval(wrap(3) == 3)          -> true,
    eval(wrap(3) eq 3)          -> true,
    eval(wrap(3) != 3)          -> false,
    eval(wrap(3) ne 3)          -> false,
    eval(wrap(3) * 2)           -> 6,
    eval(wrap(1) + 1)           -> 2,
    eval(wrap(10) - 5)          -> 5,
    eval(wrap(10) / 2)          -> 5,
    eval(wrap(20) % 3)          -> 2,
    eval(wrap(20) > 3)          -> true,
    eval(wrap(20 < 3))          -> false,
    eval(wrap(20) >= 3)         -> true,
    eval(wrap(20 <= (3: Byte))) -> false,
    eval(10.0 + (5: Char))      -> 15,

    // long
    eval(wrap(3L) * 2L)   -> 6L,
    eval(wrap(1L) + 1L)   -> 2L,
    eval(wrap(10L) - 5L)  -> 5L,
    eval(wrap(10L) / 2L)  -> 5L,
    eval(wrap(20L) % 3L)  -> 2L,
    eval(wrap(20L) > 3)   -> true,
    eval(wrap(20 < 3L))   -> false,
    eval(wrap(20) >= 3)   -> true,
    eval(wrap(20.0 <= 3)) -> false,

    // double
    eval(wrap(3.0) * 2.0)  -> 6.0,
    eval(wrap(1.5) + 1.5)  -> 3.0,
    eval(wrap(10.0) - 5.5) -> 4.5,
    eval(wrap(10.0) / 2.0) -> 5.0,
    eval(wrap(20.0) % 3.0) -> 2.0,

    // string expressions
    eval(wrap("Hello, ") + "world!")                  -> "Hello, world!",
    eval("Hello, " ++ "world!")                       -> "Hello, world!",
    eval("Scala is good".toUpperCase)                 -> "SCALA IS GOOD",
    eval("SCALA IS GOOD".toLowerCase)                 -> "scala is good",
    eval("Scala is good".toUpperCase())               -> "SCALA IS GOOD",
    eval("SCALA IS GOOD".toLowerCase())               -> "scala is good",
    eval("myemail@gmail.com".matches(".*@gmail.com")) -> true,

    // boolean expressions
    eval(wrap(true) && false) -> false,
    eval(wrap(true) || false) -> true,
    eval(!wrap(true))         -> false,
    eval(!wrap(false))        -> true,
    eval(wrap(true) == false) -> false,

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

    // list expressions
    eval(List(1, 2, 3))      -> List(1, 2, 3),
    eval(List(1, 2, 3) :+ 4) -> List(1, 2, 3, 4),
    eval(5 :: List(1, 2, 3)) -> List(5, 1, 2, 3),
    eval(List(1, 2, 3).head) -> 1,

    // asInstanceOf/isInstanceOf
    eval(List(1).isInstanceOf[List[Int]]) -> true,
    eval(List(1).asInstanceOf[List[Int]]) -> List(1),
    eval {
      val x = 10
      if x > 5 then x else "hello"
    } -> 10
  )
