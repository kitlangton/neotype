package comptime

import zio.test.*

object ExtraExpressionsJvmSpec extends ZIOSpecDefault:
  val spec =
    suite("ExtraExpressionsJvmSpec (comptime)")(
      test("placeholder") {
        assertTrue(true)
      }
      // TODO: Re-enable when String | Boolean can be lifted to Expr
      // test("java.net URI/URL") {
      //   assertTrue(
      //     comptime {
      //       val input = "http://example.com"
      //       if scala.util.Try(new java.net.URI(input)).isFailure then "Must be a valid URI" else true
      //     } == true,
      //     comptime {
      //       val input = "not a uri"
      //       if scala.util.Try(new java.net.URI(input)).isFailure then "Must be a valid URI" else true
      //     } == "Must be a valid URI",
      //     comptime {
      //       val input = "http://example.com"
      //       if scala.util.Try(new java.net.URI(input).toURL).isFailure then "Must be a valid URL" else true
      //     } == true,
      //     comptime {
      //       val input = "not a url"
      //       if scala.util.Try(new java.net.URI(input).toURL).isFailure then "Must be a valid URL" else true
      //     } == "Must be a valid URL"
      //   )
      // }
    )
