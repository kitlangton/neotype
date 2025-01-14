package neotype.eval

import neotype.TestMacros.*
import zio.test.*

import scala.util.Try

object PrimitiveTypesSpec extends ZIOSpecDefault:
  val spec =
    suite("PrimitiveTypesSpec")(
      evalTests.map { case (actual, expected) =>
        test(s"eval($actual) == $expected") {
          assertTrue(actual == expected)
        }
      }
    )

  inline def wrap[A](a: A): A = a

  lazy val evalTests = List(
    // Int operations
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

    // Long operations
    eval(wrap(3L) * 2L)   -> 6L,
    eval(wrap(1L) + 1L)   -> 2L,
    eval(wrap(10L) - 5L)  -> 5L,
    eval(wrap(10L) / 2L)  -> 5L,
    eval(wrap(20L) % 3L)  -> 2L,
    eval(wrap(20L) > 3)   -> true,
    eval(wrap(20 < 3L))   -> false,
    eval(wrap(20) >= 3)   -> true,
    eval(wrap(20.0 <= 3)) -> false,

    // Double operations
    eval(wrap(3.0) * 2.0)  -> 6.0,
    eval(wrap(1.5) + 1.5)  -> 3.0,
    eval(wrap(10.0) - 5.5) -> 4.5,
    eval(wrap(10.0) / 2.0) -> 5.0,
    eval(wrap(20.0) % 3.0) -> 2.0,

    // String operations
    eval(wrap("Hello, ") + "world!")                  -> "Hello, world!",
    eval("Hello, " ++ "world!")                       -> "Hello, world!",
    eval("Scala is good".toUpperCase)                 -> "SCALA IS GOOD",
    eval("SCALA IS GOOD".toLowerCase)                 -> "scala is good",
    eval("Scala is good".toUpperCase())               -> "SCALA IS GOOD",
    eval("SCALA IS GOOD".toLowerCase())               -> "scala is good",
    eval("myemail@gmail.com".matches(".*@gmail.com")) -> true,
    eval("UPPERCASE".forall(_.isUpper))               -> true,
    eval("abcdef".zipWithIndex.toList)                -> List(('a', 0), ('b', 1), ('c', 2), ('d', 3), ('e', 4), ('f', 5)),

    // Boolean operations
    eval(wrap(true) && false) -> false,
    eval(wrap(true) || false) -> true,
    eval(!wrap(true))         -> false,
    eval(!wrap(false))        -> true,
    eval(wrap(true) == false) -> false,

    // BigDecimal operations
    eval(BigDecimal("123.456")) -> BigDecimal(123.456),
    eval(BigDecimal(123.456))   -> BigDecimal(123.456),
    eval(BigDecimal(123))       -> BigDecimal(123),
    eval(BigDecimal(123L))      -> BigDecimal(123),
    eval(BigDecimal(123.456d))  -> BigDecimal(123.456),

    // Try operations
    eval(Try(1 / 1))                 -> scala.util.Success(1),
    eval(Try(BigDecimal("123.456"))) -> scala.util.Success(BigDecimal(123.456)),

    // Type operations
    eval(List(1).isInstanceOf[List[Int]]) -> true,
    eval(List(1).asInstanceOf[List[Int]]) -> List(1)
  )
