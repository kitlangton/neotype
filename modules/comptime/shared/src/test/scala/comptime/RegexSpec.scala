package comptime

import zio.test.*

object RegexSpec extends ZIOSpecDefault:
  val spec =
    suite("RegexSpec (comptime)")(
      suite("findFirstIn")(
        test("find first match") {
          assertTrue(
            comptime("[0-9]+".r.findFirstIn("abc123def456")) == Some("123"),
            comptime("[0-9]+".r.findFirstIn("no digits")) == None,
            comptime("a+".r.findFirstIn("baaab")) == Some("aaa")
          )
        }
      ),
      suite("findAllIn")(
        test("find all matches as iterator") {
          assertTrue(
            comptime("[0-9]+".r.findAllIn("a1b2c3").toList) == List("1", "2", "3"),
            comptime("[a-z]+".r.findAllIn("123").toList) == List(),
            comptime("a+".r.findAllIn("aabaaab").toList) == List("aa", "aaa")
          )
        }
      ),
      suite("findFirstMatchIn")(
        test("find first Match object") {
          assertTrue(
            comptime("[0-9]+".r.findFirstMatchIn("abc123def").map(_.matched)) == Some("123"),
            comptime("[0-9]+".r.findFirstMatchIn("abc123def").map(_.start)) == Some(3),
            comptime("[0-9]+".r.findFirstMatchIn("abc123def").map(_.end)) == Some(6),
            comptime("[0-9]+".r.findFirstMatchIn("no digits").isEmpty) == true
          )
        }
      ),
      suite("findAllMatchIn")(
        test("find all Match objects as iterator") {
          assertTrue(
            comptime("[0-9]+".r.findAllMatchIn("a1b22c333").map(_.matched).toList) == List("1", "22", "333"),
            comptime("[0-9]+".r.findAllMatchIn("a1b22c333").map(_.start).toList) == List(1, 3, 6),
            comptime("[0-9]+".r.findAllMatchIn("none").toList.isEmpty) == true
          )
        }
      ),
      suite("replaceAllIn")(
        test("replace all matches") {
          assertTrue(
            comptime("[0-9]+".r.replaceAllIn("a1b2c3", "X")) == "aXbXcX",
            comptime("\\s+".r.replaceAllIn("hello   world", " ")) == "hello world",
            comptime("[aeiou]".r.replaceAllIn("hello", "*")) == "h*ll*"
          )
        }
      ),
      suite("replaceFirstIn")(
        test("replace first match") {
          assertTrue(
            comptime("[0-9]+".r.replaceFirstIn("a1b2c3", "X")) == "aXb2c3",
            comptime("\\s+".r.replaceFirstIn("hello   world  test", " ")) == "hello world  test",
            comptime("[aeiou]".r.replaceFirstIn("hello", "*")) == "h*llo"
          )
        }
      ),
      suite("split")(
        test("split string by regex") {
          assertTrue(
            comptime("\\s+".r.split("hello world test").toList) == List("hello", "world", "test"),
            comptime("[,;]".r.split("a,b;c").toList) == List("a", "b", "c"),
            comptime("-+".r.split("a--b---c").toList) == List("a", "b", "c")
          )
        }
      ),
      suite("matches")(
        test("test if regex matches entire string") {
          assertTrue(
            comptime("[0-9]+".r.matches("12345")) == true,
            comptime("[0-9]+".r.matches("123abc")) == false,
            comptime(".*".r.matches("anything")) == true
          )
        }
      )
    )
