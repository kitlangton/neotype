package neotype

import zio.test.*

/** Tests for runtime validation behavior via .make() and .makeOrThrow() These
  * complement the compile-time tests in NewtypeSpec.
  */
object RuntimeValidationSpec extends ZIOSpecDefault:
  val spec = suite("RuntimeValidationSpec")(
    suite(".make() error messages")(
      test("default error message when validate returns false") {
        val result = PositiveIntNewtype.make(-1)
        assertTrue(result == Left("Validation Failed"))
      },
      test("custom error message when validate returns String") {
        val result = NonBlankStringNewtype.make("   ")
        assertTrue(result == Left("Must not be empty"))
      },
      test("custom error message for URI validation") {
        val result = UriStringNewtype.make("not a valid uri with spaces")
        assertTrue(result == Left("Must be a valid URI"))
      },
      test("custom error message for UUID validation") {
        val result = UuidStringNewtype.make("not-a-uuid")
        assertTrue(result == Left("Must be a valid UUID"))
      },
      test("custom error message for pattern match validation") {
        val result = MatchListNewtype.make(List(5, 3, 1))
        assertTrue(result == Left("Must start with an increasing pair"))
      },
      test("success returns Right with wrapped value") {
        val result = PositiveIntNewtype.make(42)
        assertTrue(result.isRight) &&
        assertTrue(result.map(_.unwrap) == Right(42))
      },
      test("subtype .make() returns same error messages") {
        val result = PositiveIntSubtype.make(-1)
        assertTrue(result == Left("Validation Failed"))
      }
    ),
    suite(".make() with dynamic values")(
      test("validates runtime-computed values") {
        val input  = List(1, 2, 3).sum // 6, computed at runtime
        val result = PositiveIntNewtype.make(input)
        assertTrue(result.isRight)
      },
      test("fails on runtime-computed invalid values") {
        val input  = List(1, 2, 3).sum - 10 // -4, computed at runtime
        val result = PositiveIntNewtype.make(input)
        assertTrue(result == Left("Validation Failed"))
      },
      test("works with Option.flatMap pattern") {
        val maybePositive: Option[PositiveIntNewtype] =
          Some(5).flatMap(n => PositiveIntNewtype.make(n).toOption)
        assertTrue(maybePositive.isDefined) &&
        assertTrue(maybePositive.get.unwrap == 5)
      },
      test("works with Either.flatMap pattern") {
        val result: Either[String, PositiveIntNewtype] =
          Right(5).flatMap(PositiveIntNewtype.make)
        assertTrue(result.isRight)
      }
    ),
    suite("WrappedType.make() consistency")(
      test("WrappedType.make matches Newtype.make") {
        val wt         = WrappedType[Int, PositiveIntNewtype]
        val direct     = PositiveIntNewtype.make(-1)
        val viaWrapped = wt.make(-1)
        assertTrue(direct == viaWrapped)
      },
      test("WrappedType.make matches Subtype.make") {
        val wt         = WrappedType[Int, PositiveIntSubtype]
        val direct     = PositiveIntSubtype.make(-1)
        val viaWrapped = wt.make(-1)
        assertTrue(direct == viaWrapped)
      }
    )
  )
