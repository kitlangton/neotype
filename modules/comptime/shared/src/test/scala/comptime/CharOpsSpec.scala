package comptime

import zio.test.*

object CharOpsSpec extends ZIOSpecDefault:
  val spec =
    suite("CharOpsSpec")(
      test("Char.isDigit") {
        assertTrue(
          comptime('5'.isDigit) == true,
          comptime('a'.isDigit) == false
        )
      },
      test("Char.isLetter") {
        assertTrue(
          comptime('a'.isLetter) == true,
          comptime('5'.isLetter) == false
        )
      },
      test("Char.isWhitespace") {
        assertTrue(
          comptime(' '.isWhitespace) == true,
          comptime('a'.isWhitespace) == false
        )
      },
      test("Char.toUpper and toLower") {
        assertTrue(
          comptime('a'.toUpper) == 'A',
          comptime('A'.toLower) == 'a'
        )
      },
      test("Char.isUpper and isLower") {
        assertTrue(
          comptime('A'.isUpper) == true,
          comptime('a'.isLower) == true
        )
      },
      test("Char.isLetterOrDigit") {
        assertTrue(
          comptime('a'.isLetterOrDigit) == true,
          comptime('5'.isLetterOrDigit) == true,
          comptime(' '.isLetterOrDigit) == false
        )
      },
      test("Char.asDigit") {
        assertTrue(
          comptime('5'.asDigit) == 5,
          comptime('A'.asDigit) == 10
        )
      }
    )
