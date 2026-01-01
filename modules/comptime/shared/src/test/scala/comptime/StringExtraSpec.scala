package comptime

import zio.test.*

object StringExtraSpec extends ZIOSpecDefault:
  val spec =
    suite("StringExtraSpec (comptime)")(
      suite("capitalize and case")(
        test("capitalize") {
          assertTrue(
            comptime("hello".capitalize) == "Hello",
            comptime("HELLO".capitalize) == "HELLO",
            comptime("".capitalize) == ""
          )
        }
      ),
      suite("strip operations")(
        test("stripPrefix") {
          assertTrue(
            comptime("hello world".stripPrefix("hello ")) == "world",
            comptime("hello world".stripPrefix("foo")) == "hello world"
          )
        },
        test("stripSuffix") {
          assertTrue(
            comptime("hello world".stripSuffix(" world")) == "hello",
            comptime("hello world".stripSuffix("foo")) == "hello world"
          )
        },
        test("stripMargin") {
          assertTrue(
            comptime("  |hello".stripMargin) == "hello",
            comptime("  #hello".stripMargin('#')) == "hello",
            comptime(
              """  |line1
                |  |line2""".stripMargin
            ) == "line1\n  |line2"
          )
        }
      ),
      suite("pad operations")(
        test("padTo") {
          assertTrue(
            comptime("hi".padTo(5, 'x')) == "hixxx",
            comptime("hello".padTo(3, 'x')) == "hello"
          )
        }
      ),
      suite("replace operations")(
        test("replace Char") {
          assertTrue(
            comptime("hello".replace('l', 'x')) == "hexxo"
          )
        },
        test("replace String") {
          assertTrue(
            comptime("hello world".replace("world", "scala")) == "hello scala"
          )
        },
        test("replaceAll") {
          assertTrue(
            comptime("hello world world".replaceAll("world", "scala")) == "hello scala scala"
          )
        },
        test("replaceFirst") {
          assertTrue(
            comptime("hello world world".replaceFirst("world", "scala")) == "hello scala world"
          )
        }
      ),
      suite("comparison operations")(
        test("compareToIgnoreCase") {
          assertTrue(
            comptime("hello".compareToIgnoreCase("HELLO")) == 0,
            comptime("a".compareToIgnoreCase("B")) < 0
          )
        },
        test("equalsIgnoreCase") {
          assertTrue(
            comptime("hello".equalsIgnoreCase("HELLO")) == true,
            comptime("hello".equalsIgnoreCase("world")) == false
          )
        }
      ),
      suite("format")(
        test("format with args") {
          assertTrue(
            comptime("%s is %d".format("answer", 42)) == "answer is 42",
            comptime("%.2f".format(3.14159)) == "3.14"
          )
        }
      ),
      suite("mkString on String")(
        test("mkString with separator") {
          assertTrue(
            comptime("abc".mkString(",")) == "a,b,c"
          )
        },
        test("mkString with prefix/sep/suffix") {
          assertTrue(
            comptime("abc".mkString("[", ",", "]")) == "[a,b,c]"
          )
        }
      ),
      suite("repeat")(
        test("String.repeat") {
          assertTrue(
            comptime("ab".repeat(3)) == "ababab",
            comptime("x".repeat(0)) == "",
            comptime("".repeat(5)) == ""
          )
        }
      ),
      suite("getBytes")(
        test("getBytes default charset") {
          assertTrue(
            comptime("hello".getBytes.toList) == "hello".getBytes.toList,
            comptime("".getBytes.toList) == List.empty[Byte]
          )
        },
        test("getBytes with charset") {
          assertTrue(
            comptime("hello".getBytes("UTF-8").toList) == "hello".getBytes("UTF-8").toList,
            comptime("café".getBytes("UTF-8").toList) == "café".getBytes("UTF-8").toList
          )
        }
      )
    )
