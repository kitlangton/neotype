package comptime

import zio.test.*

object CaseClassLiftingSpec extends ZIOSpecDefault:
  val spec =
    suite("CaseClassLiftingSpec")(
      test("lift simple case class") {
        // This tests that comptime can return a case class
        val result = comptime {
          Person("Alice", 30)
        }
        assertTrue(
          result == Person("Alice", 30),
          result.name == "Alice",
          result.age == 30
        )
      },
      test("lift case class with string operations") {
        val result = comptime {
          val name   = "Bob"
          val ageStr = "25"
          Person(name, ageStr.toInt)
        }
        assertTrue(
          result == Person("Bob", 25)
        )
      },
      test("lift case class with computation") {
        val result = comptime {
          val name = "Charlie".toLowerCase
          val age  = 10 + 15
          Person(name, age)
        }
        assertTrue(
          result == Person("charlie", 25)
        )
      },
      test("lift nested case class in option") {
        val result = comptime {
          Option(Person("Dave", 40))
        }
        assertTrue(
          result == Some(Person("Dave", 40))
        )
      },
      test("lift case class from match") {
        val result = comptime {
          val x = 1
          x match
            case 1 => Person("One", 1)
            case _ => Person("Other", 0)
        }
        assertTrue(
          result == Person("One", 1)
        )
      }
    )
