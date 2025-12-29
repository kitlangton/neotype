package neotype

import zio.test.*

/** Tests proving Subtype[A] actually behaves as a subtype of A. This is the key
  * semantic difference from Newtype.
  */
object SubtypeSemanticsSpec extends ZIOSpecDefault:

  // Test fixtures
  type PositiveInt = PositiveInt.Type
  object PositiveInt extends Subtype[Int]:
    override inline def validate(n: Int): Boolean = n > 0

  type NonEmptyString = NonEmptyString.Type
  object NonEmptyString extends Subtype[String]:
    override inline def validate(s: String): Boolean = s.nonEmpty

  type LongString = LongString.Type
  object LongString extends Subtype[String]:
    override inline def validate(s: String): Boolean = s.length > 10

  val spec = suite("SubtypeSemanticsSpec")(
    suite("Subtype is assignable to its base type")(
      test("PositiveInt is assignable to Int") {
        val positive: PositiveInt = PositiveInt(42)
        val asInt: Int            = positive // This compiles because PositiveInt <: Int
        assertTrue(asInt == 42)
      },
      test("NonEmptyString is assignable to String") {
        val nonEmpty: NonEmptyString = NonEmptyString("hello")
        val asString: String         = nonEmpty // This compiles because NonEmptyString <: String
        assertTrue(asString == "hello")
      },
      test("can pass Subtype to function expecting base type") {
        def takesInt(n: Int): Int = n * 2
        val positive: PositiveInt = PositiveInt(21)
        val result                = takesInt(positive) // Works without unwrap!
        assertTrue(result == 42)
      },
      test("can use String methods directly on String Subtype") {
        val nonEmpty: NonEmptyString = NonEmptyString("hello")
        // These work because NonEmptyString <: String
        assertTrue(nonEmpty.length == 5) &&
        assertTrue(nonEmpty.toUpperCase == "HELLO") &&
        assertTrue(nonEmpty.startsWith("hel"))
      },
      test("can use Int methods directly on Int Subtype") {
        val positive: PositiveInt = PositiveInt(42)
        // These work because PositiveInt <: Int
        assertTrue(positive.abs == 42) &&
        assertTrue(positive.toDouble == 42.0) &&
        assertTrue((positive + 8) == 50)
      }
    ),
    suite("Subtype in collections")(
      test("List[Subtype] can be used where List[Base] is expected") {
        val positives: List[PositiveInt] = List(PositiveInt(1), PositiveInt(2), PositiveInt(3))
        // Can assign to List[Int] because PositiveInt <: Int and List is covariant
        val asInts: List[Int] = positives
        assertTrue(asInts.sum == 6)
      },
      test("can use collection operations that return base type") {
        val positives: List[PositiveInt] = List(PositiveInt(1), PositiveInt(2), PositiveInt(3))
        val sum: Int                     = positives.sum // Returns Int, not PositiveInt
        assertTrue(sum == 6)
      },
      test("Option[Subtype] is assignable to Option[Base]") {
        val maybePositive: Option[PositiveInt] = Some(PositiveInt(42))
        val maybeInt: Option[Int]              = maybePositive
        assertTrue(maybeInt.contains(42))
      }
    ),
    suite("Subtype vs Newtype behavior difference")(
      test("Newtype requires explicit unwrap") {
        val newtype: PositiveIntNewtype = PositiveIntNewtype(42)
        // val asInt: Int = newtype // This would NOT compile!
        val asInt: Int = newtype.unwrap // Must unwrap
        assertTrue(asInt == 42)
      },
      test("Subtype does NOT require unwrap") {
        val subtype: PositiveInt = PositiveInt(42)
        val asInt: Int           = subtype // Compiles directly!
        assertTrue(asInt == 42)
      },
      test("both have the same validation behavior") {
        assertTrue(PositiveIntNewtype.make(-1).isLeft) &&
        assertTrue(PositiveInt.make(-1).isLeft) &&
        assertTrue(PositiveIntNewtype.make(1).isRight) &&
        assertTrue(PositiveInt.make(1).isRight)
      }
    ),
    suite("Subtype preserves type safety")(
      test("different Subtypes of same base are not interchangeable") {
        val positive: PositiveInt = PositiveInt(5)
        // val nonEmpty: NonEmptyString = positive // Would not compile - different types

        // But both can be used as their base types
        val _: Int                   = positive
        val nonEmpty: NonEmptyString = NonEmptyString("hello")
        val _: String                = nonEmpty
        assertTrue(true)
      },
      test("cannot assign base type to Subtype") {
        // val positive: PositiveInt = 42 // Would not compile!
        // Must go through constructor or make
        val positive = PositiveInt(42)
        assertTrue(positive == 42)
      }
    ),
    suite("Stacked Subtypes")(
      // TODO: Issue #372 - stacked subtype creation needs Cents(100L) to be comptime-evaluable
      // test("Subtype of Subtype maintains both constraints") {
      //   val nnc: NonNegativeCents = NonNegativeCents(Cents(100L))
      //   val asCents: Cents = nnc
      //   assertTrue(asCents.unwrap == 100L)
      // },
      test("Subtype of Subtype enforces both validations at runtime") {
        // Can't create NonNegativeCents with negative value
        val result = NonNegativeCents.make(Cents(-1L))
        assertTrue(result.isLeft)
      }
    )
  )
