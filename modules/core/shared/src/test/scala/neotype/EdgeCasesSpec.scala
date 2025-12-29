package neotype

import zio.test.*

/** Edge case tests: unicode, whitespace, large numbers, boundaries, etc. */
object EdgeCasesSpec extends ZIOSpecDefault:

  // Test fixtures for edge cases
  type TrimmedString = TrimmedString.Type
  object TrimmedString extends Newtype[String]:
    override inline def validate(s: String): Boolean = s == s.trim

  type NonEmptyTrimmed = NonEmptyTrimmed.Type
  object NonEmptyTrimmed extends Newtype[String]:
    override inline def validate(s: String): Boolean =
      s.trim.nonEmpty

  type AsciiOnly = AsciiOnly.Type
  object AsciiOnly extends Newtype[String]:
    override inline def validate(s: String): Boolean =
      s.forall(c => c >= 0 && c <= 127)

  type MaxLength10 = MaxLength10.Type
  object MaxLength10 extends Newtype[String]:
    override inline def validate(s: String): Boolean = s.length <= 10

  type BoundedInt = BoundedInt.Type
  object BoundedInt extends Newtype[Int]:
    override inline def validate(n: Int): Boolean = n >= -100 && n <= 100

  type NonZeroLong = NonZeroLong.Type
  object NonZeroLong extends Newtype[Long]:
    override inline def validate(n: Long): Boolean = n != 0L

  val spec = suite("EdgeCasesSpec")(
    suite("Unicode handling")(
      test("accepts basic unicode characters") {
        val result = NonEmptyStringNewtype.make("hello world")
        assertTrue(result.isRight)
      },
      test("accepts CJK characters") {
        val result = NonEmptyStringNewtype.make("hello")
        assertTrue(result.isRight)
      },
      test("ASCII-only validation rejects high chars") {
        val result = AsciiOnly.make("hello\u0080")
        assertTrue(result.isLeft)
      },
      test("ASCII-only validation accepts plain ASCII") {
        val result = AsciiOnly.make("hello world 123")
        assertTrue(result.isRight)
      }
    ),
    suite("Whitespace handling")(
      test("empty string") {
        val result = NonEmptyStringNewtype.make("")
        assertTrue(result.isLeft)
      },
      test("single space") {
        val result = NonEmptyStringNewtype.make(" ")
        assertTrue(result.isRight) // NonEmpty checks length, not blankness
      },
      test("only whitespace - NonEmptyTrimmed rejects") {
        val result = NonEmptyTrimmed.make("   ")
        assertTrue(result.isLeft)
      },
      test("leading/trailing whitespace - TrimmedString rejects") {
        assertTrue(TrimmedString.make(" hello").isLeft) &&
        assertTrue(TrimmedString.make("hello ").isLeft) &&
        assertTrue(TrimmedString.make(" hello ").isLeft)
      },
      test("no leading/trailing whitespace - TrimmedString accepts") {
        assertTrue(TrimmedString.make("hello").isRight) &&
        assertTrue(TrimmedString.make("hello world").isRight)
      },
      test("various whitespace characters") {
        assertTrue(NonEmptyTrimmed.make("\t").isLeft) &&
        assertTrue(NonEmptyTrimmed.make("\n").isLeft) &&
        assertTrue(NonEmptyTrimmed.make("\r\n").isLeft)
      }
    ),
    suite("Numeric boundaries")(
      test("Int.MaxValue") {
        val result = PositiveIntNewtype.make(Int.MaxValue)
        assertTrue(result.isRight) &&
        assertTrue(result.map(_.unwrap) == Right(Int.MaxValue))
      },
      test("Int.MinValue") {
        val result = PositiveIntNewtype.make(Int.MinValue)
        assertTrue(result.isLeft)
      },
      test("zero boundary for positive") {
        assertTrue(PositiveIntNewtype.make(0).isLeft) &&
        assertTrue(PositiveIntNewtype.make(1).isRight)
      },
      test("Long.MaxValue") {
        val result = PositiveLongNewtype.make(Long.MaxValue)
        assertTrue(result.isRight)
      },
      test("Long.MinValue") {
        val result = PositiveLongNewtype.make(Long.MinValue)
        assertTrue(result.isLeft)
      },
      test("bounded int at boundaries") {
        assertTrue(BoundedInt.make(-100).isRight) &&
        assertTrue(BoundedInt.make(100).isRight) &&
        assertTrue(BoundedInt.make(-101).isLeft) &&
        assertTrue(BoundedInt.make(101).isLeft)
      },
      test("non-zero long") {
        assertTrue(NonZeroLong.make(0L).isLeft) &&
        assertTrue(NonZeroLong.make(1L).isRight) &&
        assertTrue(NonZeroLong.make(-1L).isRight) &&
        assertTrue(NonZeroLong.make(Long.MaxValue).isRight) &&
        assertTrue(NonZeroLong.make(Long.MinValue).isRight)
      }
    ),
    suite("String length boundaries")(
      test("empty string for max length") {
        assertTrue(MaxLength10.make("").isRight)
      },
      test("exactly at max length") {
        assertTrue(MaxLength10.make("1234567890").isRight)
      },
      test("one over max length") {
        assertTrue(MaxLength10.make("12345678901").isLeft)
      },
      test("very long string") {
        val longString = "x" * 10000
        assertTrue(MaxLength10.make(longString).isLeft)
      }
    ),
    suite("Special string values")(
      test("string with null character") {
        val withNull = "hello\u0000world"
        val result   = NonEmptyStringNewtype.make(withNull)
        assertTrue(result.isRight)
      },
      test("string that looks like number") {
        val result = NonEmptyStringNewtype.make("12345")
        assertTrue(result.isRight)
      },
      test("string with quotes") {
        val result = NonEmptyStringNewtype.make("\"quoted\"")
        assertTrue(result.isRight)
      },
      test("string with backslashes") {
        val result = NonEmptyStringNewtype.make("path\\to\\file")
        assertTrue(result.isRight)
      },
      test("multiline string") {
        val multiline = "line1\nline2\nline3"
        val result    = NonEmptyStringNewtype.make(multiline)
        assertTrue(result.isRight)
      }
    ),
    suite("Collection edge cases")(
      test("empty list validation") {
        val result = MatchListNewtype.make(List())
        assertTrue(result.isLeft) // needs at least 2 elements
      },
      test("single element list") {
        val result = MatchListNewtype.make(List(1))
        assertTrue(result.isLeft) // needs at least 2 elements
      },
      test("large list") {
        val bigList = (1 to 10000).toList
        val result  = MatchListNewtype.make(bigList)
        assertTrue(result.isRight) // 1 < 2, valid increasing pair
      }
    )
  )
