package comptime

import zio.test.*

object CharSpec extends ZIOSpecDefault:
  val spec =
    suite("CharSpec (comptime)")(
      suite("Char basics")(
        test("toUpper") {
          assertTrue(
            comptime('a'.toUpper) == 'A',
            comptime('z'.toUpper) == 'Z',
            comptime('A'.toUpper) == 'A'
          )
        },
        test("toLower") {
          assertTrue(
            comptime('A'.toLower) == 'a',
            comptime('Z'.toLower) == 'z',
            comptime('a'.toLower) == 'a'
          )
        },
        test("isDigit") {
          assertTrue(
            comptime('5'.isDigit) == true,
            comptime('a'.isDigit) == false
          )
        },
        test("isLetter") {
          assertTrue(
            comptime('a'.isLetter) == true,
            comptime('5'.isLetter) == false
          )
        },
        test("isUpper") {
          assertTrue(
            comptime('A'.isUpper) == true,
            comptime('a'.isUpper) == false
          )
        },
        test("isLower") {
          assertTrue(
            comptime('a'.isLower) == true,
            comptime('A'.isLower) == false
          )
        },
        test("asDigit") {
          assertTrue(
            comptime('5'.asDigit) == 5,
            comptime('0'.asDigit) == 0,
            comptime('9'.asDigit) == 9
          )
        },
        test("toInt") {
          assertTrue(
            comptime('A'.toInt) == 65,
            comptime('0'.toInt) == 48
          )
        }
      ),
      suite("Char arithmetic")(
        test("Char + Int") {
          assertTrue(
            comptime('a' + 1) == 98,
            comptime('A' + 25) == 90
          )
        },
        test("Char - Int") {
          assertTrue(
            comptime('b' - 1) == 97,
            comptime('Z' - 25) == 65
          )
        },
        test("Char - Char") {
          assertTrue(
            comptime('b' - 'a') == 1,
            comptime('z' - 'a') == 25
          )
        }
      ),
      suite("Char ranges")(
        test("to") {
          assertTrue(
            comptime('a'.to('c').toList) == List('a', 'b', 'c'),
            comptime('A'.to('C').toList) == List('A', 'B', 'C')
          )
        },
        test("until") {
          assertTrue(
            comptime('a'.until('c').toList) == List('a', 'b'),
            comptime('A'.until('D').toList) == List('A', 'B', 'C')
          )
        }
      ),
      suite("String.map with Char")(
        test("map toUpper") {
          assertTrue(
            comptime("hello".map(_.toUpper)) == "HELLO"
          )
        },
        test("map toLower") {
          assertTrue(
            comptime("HELLO".map(_.toLower)) == "hello"
          )
        },
        test("map toInt returns collection") {
          // When map returns non-Char, the result is a collection of Int
          // We convert to List to avoid type inference issues
          assertTrue(
            comptime("abc".map(_.toInt).toList) == List(97, 98, 99)
          )
        },
        test("map on empty string") {
          // Matches Scala 3 exactly using type args from AST:
          //   "".map(_.toUpper) returns "" (String)
          //   "".map(_.toInt)   returns empty IndexedSeq[Int]
          assertTrue(
            comptime("".map(_.toUpper)) == "",
            comptime("".map(_.toUpper).toList) == Nil,
            comptime("".map(_.toInt).toList) == Nil
          )
        },
        test("map with Char comparison and arithmetic") {
          // This tests the Char widening fix - Char comparisons/arithmetic are typed as Int ops
          inline def hexDigit(c: Char): Int =
            if c >= '0' && c <= '9' then c - '0'
            else if c >= 'a' && c <= 'f' then c - 'a' + 10
            else if c >= 'A' && c <= 'F' then c - 'A' + 10
            else -1
          assertTrue(
            comptime("ff".map(c => hexDigit(c)).toList) == List(15, 15),
            comptime("09aF".map(c => hexDigit(c)).toList) == List(0, 9, 10, 15)
          )
        }
      ),
      suite("String.flatMap with Char")(
        test("flatMap returning String") {
          // flatMap(f: Char => String): String - no type param
          assertTrue(
            comptime("hi".flatMap(c => s"$c!")) == "h!i!"
          )
        },
        test("flatMap returning IndexedSeq") {
          // flatMap[B](f: Char => IterableOnce[B]): IndexedSeq[B] - has type param
          assertTrue(
            comptime("ab".flatMap(c => List(c.toInt)).toList) == List(97, 98)
          )
        },
        test("flatMap on empty string") {
          assertTrue(
            comptime("".flatMap(c => s"$c!")) == ""
          )
        }
      ),
      suite("String.collect")(
        test("collect returning String") {
          // collect(pf: PartialFunction[Char, Char]): String - no type param
          assertTrue(
            comptime("hello".collect { case c if c != 'l' => c.toUpper }) == "HEO"
          )
        },
        test("collect returning IndexedSeq") {
          // collect[B](pf: PartialFunction[Char, B]): IndexedSeq[B] - has type param
          assertTrue(
            comptime("abc".collect { case c => c.toInt }.toList) == List(97, 98, 99)
          )
        },
        test("collect on empty string") {
          assertTrue(
            comptime("".collect { case c => c.toUpper }) == ""
          )
        }
      )
    )
