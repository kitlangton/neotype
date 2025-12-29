package neotype

import zio.test.*

object CommonMakeSpec extends ZIOSpecDefault:
  private val validUuidLower = "550e8400-e29b-41d4-a716-446655440000"
  private val validUuidUpper = "550E8400-E29B-41D4-A716-446655440000"

  val spec = suiteAll("CommonMakeSpec") {
    suiteAll("Subtype.make") {
      test("common.NonEmptyString success") {
        val res = neotype.common.NonEmptyString.make("abcd")
        assertTrue(res.contains("abcd"))
      }

      test("common.NonEmptyString failure (blank)") {
        val res = neotype.common.NonEmptyString.make("   ")
        assertTrue(res == Left("NonEmptyString cannot be empty"))
      }

      test("common.NonEmptyString failure (empty)") {
        val res = neotype.common.NonEmptyString.make("")
        assertTrue(res == Left("NonEmptyString cannot be empty"))
      }

      test("common.UriString success") {
        val res = neotype.common.UriString.make("http://example.com")
        assertTrue(res.contains("http://example.com"))
      }

      test("common.UriString failure") {
        val res = neotype.common.UriString.make("hello world")
        assertTrue(res == Left("Must be a valid URI"))
      }

      test("common.UuidString success (lowercase)") {
        val res = neotype.common.UuidString.make(validUuidLower)
        assertTrue(res.contains(validUuidLower))
      }

      test("common.UuidString success (uppercase)") {
        val res = neotype.common.UuidString.make(validUuidUpper)
        assertTrue(res.contains(validUuidUpper))
      }

      test("common.UuidString failure") {
        val res = neotype.common.UuidString.make("not-a-uuid")
        assertTrue(res == Left("Must be a valid UUID"))
      }
    } +
      suiteAll("Newtype.make") {
        test("NonBlankStringNewtype success") {
          val res = NonBlankStringNewtype.make("abcd")
          assertTrue(res.map(_.unwrap) == Right("abcd"))
        }

        test("NonBlankStringNewtype failure (blank)") {
          val res = NonBlankStringNewtype.make("   ")
          assertTrue(res == Left("Must not be empty"))
        }

        test("UriStringNewtype success") {
          val res = UriStringNewtype.make("http://example.com")
          assertTrue(res.map(_.unwrap) == Right("http://example.com"))
        }

        test("UriStringNewtype failure") {
          val res = UriStringNewtype.make("hello world")
          assertTrue(res == Left("Must be a valid URI"))
        }

        test("UuidStringNewtype success (lowercase)") {
          val res = UuidStringNewtype.make(validUuidLower)
          assertTrue(res.map(_.unwrap) == Right(validUuidLower))
        }

        test("UuidStringNewtype success (uppercase)") {
          val res = UuidStringNewtype.make(validUuidUpper)
          assertTrue(res.map(_.unwrap) == Right(validUuidUpper))
        }

        test("UuidStringNewtype failure") {
          val res = UuidStringNewtype.make("not-a-uuid")
          assertTrue(res == Left("Must be a valid UUID"))
        }
      }
  }
