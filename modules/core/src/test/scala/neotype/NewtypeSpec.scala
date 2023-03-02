package neotype

import scala.quoted.*
import zio.test.*

import scala.compiletime.summonInline
import scala.compiletime.testing.*

object NewtypeSpec extends ZIOSpecDefault:
  val spec = suite("NewtypeSpec")(
    test("parse success") {
      val res = RegexNewtype("abcd")
      assertTrue(res.unwrap == "abcd")
    },
    test("parse failure") {
      val res = typeCheckErrors("""RegexNewtype("-")""").head
      assertTrue(res.message contains "Newtype Error")
    },

    // Positive
    test("positive success") {
      val res = PositiveIntNewtype(1)
      assertTrue(res.unwrap == 1)
    },
    test("positive failure") {
      val res = typeCheckErrors("""PositiveIntNewtype(-1)""").head
      assertTrue(res.message contains "Newtype Error")
    },

    // Positive Long
    test("positive long success") {
      val res = PositiveLongNewtype(1L)
      assertTrue(res.unwrap == 1L)
    },
    test("positive long failure") {
      val res = typeCheckErrors("""PositiveLongNewtype(-1L)""").head
      assertTrue(res.message contains "Newtype Error")
    },

    // NonEmpty
    test("non empty success") {
      val res = NonEmptyStringNewtype("abcd")
      assertTrue(res.unwrap == "abcd")
    },
    test("non empty failure") {
      val res = typeCheckErrors(""" NonEmptyStringNewtype("") """).head
      assertTrue(res.message contains "Newtype Error")
    },
    test("success") {
      val res = PositiveIntNewtype.applyAll(1, 2)
      assertTrue(res.map(PositiveIntNewtype.unwrap(_)) == List(1, 2))
    },
    test("isUuid success") {
      val res = IsUUID("a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a21")
      assertTrue(IsUUID.unwrap(res) == "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a21")
    },
    test("isUuid failure") {
      val res = typeCheckErrors(""" IsUUID("oops") """).head
      assertTrue(res.message contains "Newtype Error")
    },
    test("isURL success") {
      val res = IsURL("https://www.google.com")
      assertTrue(IsURL.unwrap(res) == "https://www.google.com")
    },
    test("isURL failure") {
      val res = typeCheckErrors(""" IsURL("google.com") """).head
      assertTrue(
        res.message contains "Newtype Error",
        !(res.message contains "Validation Failed")
      )
    },

    // custom failure message
    test("custom failure message") {
      val res = typeCheckErrors(""" CustomFailureNewtype("hello") """).head
      assertTrue(res.message contains "Must be the secret string!")
    },
    // test error messages when cannot parse at compile-time

    suite(".make")(
      test("success") {
        val res = PositiveIntNewtype.make(1)
        assertTrue(res.map(PositiveIntNewtype.unwrap(_)) == Right(1))
      },
      test("failure") {
        val res = PositiveIntNewtype.make(-1)
        assertTrue(res.map(PositiveIntNewtype.unwrap(_)) == Left("Validation Failed"))
      }
    )
  )
