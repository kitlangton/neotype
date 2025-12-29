package neotype

import zio.test.*

import scala.compiletime.testing.*

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

    // neotype.common.NonEmptyString
    test("common.NonEmptyString failure (whitespace)") {
      val res = typeCheckErrors(""" neotype.common.NonEmptyString("   ") """).head
      assertTrue(res.message contains "NonEmptyString cannot be empty")
    }

    // NonBlankStringNewtype
    test("non blank success") {
      val res = NonBlankStringNewtype("abcd")
      assertTrue(res.unwrap == "abcd")
    }

    test("non blank failure (whitespace)") {
      val res = typeCheckErrors(""" NonBlankStringNewtype("   ") """).head
      assertTrue(res.message contains "Must not be empty")
    }

    // UriStringNewtype
    test("uri success") {
      val res = UriStringNewtype("http://example.com")
      assertTrue(res.unwrap == "http://example.com")
    }

    test("uri failure") {
      val res = typeCheckErrors(""" UriStringNewtype("hello world") """).head
      assertTrue(res.message contains "Must be a valid URI")
    }

    // neotype.common.UriString
    test("common.UriString success") {
      val res = neotype.common.UriString("http://example.com")
      assertTrue(res == "http://example.com")
    }

    test("common.UriString failure") {
      val res = typeCheckErrors(""" neotype.common.UriString("hello world") """).head
      assertTrue(res.message contains "Must be a valid URI")
    }

    // UuidStringNewtype
    test("uuid success") {
      val res = UuidStringNewtype("550e8400-e29b-41d4-a716-446655440000")
      assertTrue(res.unwrap == "550e8400-e29b-41d4-a716-446655440000")
    }

    test("uuid success (uppercase input)") {
      val res = UuidStringNewtype("550E8400-E29B-41D4-A716-446655440000")
      assertTrue(res.unwrap == "550E8400-E29B-41D4-A716-446655440000")
    }

    test("uuid failure") {
      val res = typeCheckErrors(""" UuidStringNewtype("not-a-uuid") """).head
      assertTrue(res.message contains "Must be a valid UUID")
    }

    // neotype.common.UuidString
    test("common.UuidString success") {
      val res = neotype.common.UuidString("550e8400-e29b-41d4-a716-446655440000")
      assertTrue(res == "550e8400-e29b-41d4-a716-446655440000")
    }

    test("common.UuidString success (uppercase input)") {
      val res = neotype.common.UuidString("550E8400-E29B-41D4-A716-446655440000")
      assertTrue(res == "550E8400-E29B-41D4-A716-446655440000")
    }

    test("common.UuidString failure") {
      val res = typeCheckErrors(""" neotype.common.UuidString("not-a-uuid") """).head
      assertTrue(res.message contains "Must be a valid UUID")
    }

    // Short-circuit boolean semantics (compile-time eval must not eagerly evaluate rhs)
    test("short-circuit || (no division by zero)") {
      val res = ShortCircuitOrNewtype(0)
      assertTrue(res.unwrap == 0)
    }

    test("short-circuit || (rhs evaluated when needed)") {
      val res = ShortCircuitOrNewtype(2)
      assertTrue(res.unwrap == 2)
    }

    test("short-circuit || (validation failure)") {
      val res = typeCheckErrors(""" ShortCircuitOrNewtype(3) """).head
      assertTrue(res.message contains "INVALID")
    }

    test("short-circuit && (validation failure, not parse failure)") {
      val res = typeCheckErrors(""" ShortCircuitAndNewtype(0) """).head
      assertTrue(res.message contains "INVALID")
      assertTrue(!(res.message contains "I've FAILED"))
    }

    test("match/unapplySeq validate success") {
      val res = MatchListNewtype(List(1, 2, 3))
      assertTrue(res.unwrap == List(1, 2, 3))
    }

    test("match/unapplySeq validate failure") {
      val res = typeCheckErrors(""" MatchListNewtype(List(2, 1)) """).head
      assertTrue(res.message contains "Must start with an increasing pair")
    }

    test("typed pattern validate success") {
      val res = TypedMatchAnyNewtype("abc")
      assertTrue(res.unwrap == "abc")
    }

    test("typed pattern validate failure") {
      val res = typeCheckErrors(""" TypedMatchAnyNewtype(123) """).head
      assertTrue(res.message contains "Must be a String")
    }

    test("spongebob validate success") {
      val res = SpongebobString("HeLlO WoRlD")
      assertTrue(res.unwrap == "HeLlO WoRlD")
    }

    test("spongebob validate failure") {
      val res = typeCheckErrors(""" SpongebobString("x") """).head
      assertTrue(res.message contains "INVALID")
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

    suiteAll("Wrappable") {
      test("wrap to newtype - success") {
        val result = Wrappable[Int, PositiveIntNewtype].wrap(1)
        assertTrue(result == Right(PositiveIntNewtype(1)))
      }

      test("wrap to newtype - failure") {
        val result = Wrappable[Int, PositiveIntNewtype].wrap(-1)
        assertTrue(result.isLeft)
      }

      test("wrap to subtype - success") {
        val result = Wrappable[Int, PositiveIntSubtype].wrap(1)
        assertTrue(result == Right(PositiveIntSubtype(1)))
      }

      test("wrap to subtype - failure") {
        val result = Wrappable[Int, PositiveIntSubtype].wrap(-1)
        assertTrue(result.isLeft)
      }
    }

    test("stacked newtype creation - Cents wrapping Long") {
      val cents: Cents = Cents(100L)
      assertTrue(cents.unwrap == 100L)
    }

    // TODO: Issue #372 - stacked subtype creation needs Cents(100L) to be comptime-evaluable
    // test("stacked subtype creation - NonNegativeCents wrapping Cents") {
    //   val nnc: NonNegativeCents = NonNegativeCents(Cents(100L))
    //   val underlying: Long = Cents.unwrap(nnc)
    //   assertTrue(underlying == 100L)
    // }

    test("stacked subtype validation - fails for negative") {
      val res = typeCheckErrors("""NonNegativeCents(Cents(-1L))""").head
      assertTrue(res.message contains "Neotype Error")
    }

    suiteAll("makeOrThrow") {
      test("newtype success") {
        val res = PositiveIntNewtype.makeOrThrow(1)
        assertTrue(res.unwrap == 1)
      }

      test("newtype failure throws IllegalArgumentException") {
        val error = scala.util.Try(PositiveIntNewtype.makeOrThrow(-1)).failed.get
        assertTrue(
          error.isInstanceOf[IllegalArgumentException],
          error.getMessage == "Validation Failed"
        )
      }

      test("subtype success") {
        val res = PositiveIntSubtype.makeOrThrow(1)
        assertTrue(res == 1)
      }

      test("subtype failure throws IllegalArgumentException") {
        val error = scala.util.Try(PositiveIntSubtype.makeOrThrow(-1)).failed.get
        assertTrue(
          error.isInstanceOf[IllegalArgumentException],
          error.getMessage == "Validation Failed"
        )
      }

      test("custom error message is preserved") {
        val error = scala.util.Try(CustomFailureNewtype.makeOrThrow("hello")).failed.get
        assertTrue(error.getMessage == "Must be the secret string!")
      }
    }
  }
