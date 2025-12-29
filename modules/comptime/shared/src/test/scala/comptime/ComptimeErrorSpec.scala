package comptime

import zio.test.*

import scala.compiletime.testing.*

object ComptimeErrorSpec extends ZIOSpecDefault:
  val spec =
    suite("ComptimeErrorSpec")(
      suite("comptimeError basics")(
        test("simple error message") {
          val errors = typeCheckErrors("""
            comptime {
              comptimeError("validation failed")
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("validation failed")
          )
        },
        test("error message with computed value") {
          val errors = typeCheckErrors("""
            comptime {
              val x = 42
              comptimeError(s"Value $x is invalid")
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Value 42 is invalid")
          )
        },
        test("error in conditional branch") {
          val errors = typeCheckErrors("""
            comptime {
              val n = -5
              if n >= 0 then n
              else comptimeError(s"Negative value: $n")
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Negative value: -5")
          )
        },
        test("error in match case") {
          val errors = typeCheckErrors("""
            comptime {
              val opt: Option[Int] = None
              opt match
                case Some(v) => v
                case None => comptimeError("Expected Some, got None")
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Expected Some, got None")
          )
        }
      ),
      suite("success cases - no errors")(
        test("valid code produces no errors") {
          val errors = typeCheckErrors("""
            comptime {
              val n = 5
              if n >= 0 then n
              else comptimeError("Negative")
            }
          """)
          assertTrue(errors.isEmpty)
        },
        test("success in match - no error path taken") {
          val errors = typeCheckErrors("""
            comptime {
              val opt: Option[Int] = Some(42)
              opt match
                case Some(v) => v
                case None => comptimeError("Expected Some")
            }
          """)
          assertTrue(errors.isEmpty)
        }
      ),
      suite("complex error messages")(
        test("multiple interpolated values") {
          val errors = typeCheckErrors("""
            comptime {
              val min = 0
              val max = 100
              val value = -5
              if value >= min && value <= max then value
              else comptimeError(s"Value $value not in range [$min, $max]")
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Value -5 not in range [0, 100]")
          )
        },
        test("error with computed expression") {
          val errors = typeCheckErrors("""
            comptime {
              val items = List("a", "b", "c")
              if items.size < 3 then items.size
              else comptimeError(s"Too many items: ${items.size}")
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Too many items: 3")
          )
        }
      ),
      suite("error in nested contexts")(
        test("error in nested block") {
          val errors = typeCheckErrors("""
            comptime {
              val result = {
                val x = 10
                if x > 20 then x
                else comptimeError("x too small")
              }
              result
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("x too small")
          )
        },
        test("error after map") {
          val errors = typeCheckErrors("""
            comptime {
              val nums = List(1, 2, 3)
              val doubled = nums.map(_ * 2)
              if doubled.sum > 20 then doubled.sum
              else comptimeError(s"Sum too small: ${doubled.sum}")
            }
          """)
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Sum too small: 12")
          )
        }
      )
    )
