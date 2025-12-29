package comptime

import zio.test.*

object StringInterpolationSpec extends ZIOSpecDefault:
  val spec =
    suite("StringInterpolationSpec")(
      suite("s interpolation")(
        test("simple interpolation") {
          val result = comptime {
            val x = 42
            s"value is $x"
          }
          assertTrue(result == "value is 42")
        },
        test("multiple variables") {
          val result = comptime {
            val a = 1
            val b = 2
            s"$a + $b = ${a + b}"
          }
          assertTrue(result == "1 + 2 = 3")
        },
        test("string variable") {
          val result = comptime {
            val name = "world"
            s"hello $name"
          }
          assertTrue(result == "hello world")
        },
        test("nested expressions") {
          val result = comptime {
            val x = 10
            s"double is ${x * 2}, square is ${x * x}"
          }
          assertTrue(result == "double is 20, square is 100")
        },
        test("empty interpolation") {
          val result = comptime("no vars here")
          assertTrue(result == "no vars here")
        },
        test("only variable") {
          val result = comptime {
            val x = "hello"
            s"$x"
          }
          assertTrue(result == "hello")
        },
        test("adjacent variables") {
          val result = comptime {
            val a = "foo"
            val b = "bar"
            s"$a$b"
          }
          assertTrue(result == "foobar")
        },
        test("interpolation with method calls") {
          val result = comptime {
            val s = "hello"
            s"upper: ${s.toUpperCase}"
          }
          assertTrue(result == "upper: HELLO")
        }
      ),
      suite("interpolation in comptimeError")(
        test("error message includes interpolated value") {
          import scala.compiletime.testing.*
          val errors = typeCheckErrors("""
            comptime {
              val x = 42
              if x > 100 then "ok"
              else comptimeError(s"Value $x is too small")
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Value 42 is too small")
          )
        },
        test("complex interpolation in error") {
          import scala.compiletime.testing.*
          val errors = typeCheckErrors("""
            comptime {
              val min = 10
              val max = 100
              val value = 5
              if value >= min && value <= max then value
              else comptimeError(s"Value $value not in range [$min, $max]")
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Value 5 not in range [10, 100]")
          )
        }
      )
    )
