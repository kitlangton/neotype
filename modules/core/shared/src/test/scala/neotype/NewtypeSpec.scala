package neotype

import zio.test.*

import scala.compiletime.summonInline
import scala.compiletime.testing.*
import scala.quoted.*

object NewtypeSpec extends ZIOSpecDefault:
  val spec = suiteAll("NewtypeSpec") {

    test("parse success") {
      val res: RegexNewtype = RegexNewtype("abcd")
      assertTrue(res.unwrap == "abcd")
    }

    test("parse failure") {
      val res = typeCheckErrors("""RegexNewtype("-")""").head
      assertTrue(res.message contains "Neotype Error")
    }

    test("specific givens") {
      val regex = RegexNewtype("abcd")
      assertTrue(regex.show == "abcd")
    }

    test("extensions") {
      val regex = RegexNewtype("abcd")
      assertTrue(regex.length == 4)
    }

    // Positive
    test("positive success") {
      val res = PositiveIntNewtype(1)
      assertTrue(res.unwrap == 1)
    }

    test("positive failure") {
      val res = typeCheckErrors("""PositiveIntNewtype(-1)""").head
      assertTrue(res.message contains "Neotype Error")
    }

    // Positive Long
    test("positive long success") {
      val res = PositiveLongNewtype(1L)
      assertTrue(res.unwrap == 1L)
    }

    test("positive long failure") {
      val res = typeCheckErrors("""PositiveLongNewtype(-1L)""").head
      assertTrue(res.message contains "Neotype Error")
    }

    // NonEmpty
    test("non empty success") {
      val res = NonEmptyStringNewtype("abcd")
      assertTrue(res.unwrap == "abcd")
    }

    test("non empty failure") {
      val res = typeCheckErrors(""" NonEmptyStringNewtype("") """).head
      assertTrue(res.message contains "Neotype Error")
    }

    test("success") {
      val res = PositiveIntNewtype.applyAll(1, 2)
      assertTrue(res.map(PositiveIntNewtype.unwrap(_)) == List(1, 2))
    }

    // custom failure message
    test("custom failure message") {
      val res = typeCheckErrors(""" CustomFailureNewtype("hello") """).head
      assertTrue(res.message contains "Must be the secret string!")
    }
    // test error messages when cannot parse at compile-time

    suiteAll(".make") {
      test("success") {
        val res = PositiveIntNewtype.make(1)
        assertTrue(res.map(PositiveIntNewtype.unwrap(_)) == Right(1))
      }

      test("failure") {
        val res = PositiveIntNewtype.make(-1)
        assertTrue(res.map(PositiveIntNewtype.unwrap(_)) == Left("Validation Failed"))
      }
    }

    suiteAll("unsafeMake") {
      test("success") {
        val res = PositiveIntNewtype.unsafeMake(1).unwrap
        assertTrue(res == 1)
      }
    }
  }
