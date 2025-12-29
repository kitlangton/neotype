package neotype

import zio.test.*

import scala.compiletime.testing.*

object ParsedSpec extends ZIOSpecDefault:
  // Simple parser - works even from same file because no complex references
  object PositiveIntFromString extends Parsed[Int]:
    override inline def parse(inline input: String): Either[String, Int] =
      input.toIntOption match
        case Some(value) if value > 0 => Right(value)
        case Some(value)              => Left(s"Must be positive, got: $value")
        case None                     => Left(s"Not an int: $input")

  val spec = suiteAll("ParsedSpec") {
    suiteAll("Parsed") {
      test("compile-time parse success") {
        val res = PositiveIntFromString("42")
        assertTrue(res == 42)
      }

      test("compile-time parse failure") {
        val res = typeCheckErrors("""PositiveIntFromString("-1")""").head
        assertTrue(
          res.message.contains("PositiveIntFromString") &&
            res.message.contains("failed to parse")
        )
      }

      test("runtime parse success") {
        val res = PositiveIntFromString.make("7")
        assertTrue(res == Right(7))
      }

      test("runtime parse failure") {
        val res = PositiveIntFromString.make("oops")
        assertTrue(res match
          case Left(message) => message.contains("Not an int")
          case Right(_)      => false)
      }
    }
  }
