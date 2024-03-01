package neotype

import zio.test.*

object SubtypeSpec extends ZIOSpecDefault:
  type LongString = LongString.Type
  given LongString: Subtype[String] with
    override inline def validate(value: String): Boolean =
      value.length > 10

    override inline def failureMessage = "String must be longer than 10 characters"

  val spec = suite("SubtypeSpec")(
    suite("unsafe")(
      test("success") {
        val res =
          LongString.unsafe:
            "Nice long expected string"
        assertTrue:
            res == "Nice long expected string"
      },
      test("failure")(
        try
          LongString.unsafe:
            "toosmall"
          assertNever:
              "Should blow up when string is small"
        catch
          case _ =>
            assertCompletes
      )
    )
  )
