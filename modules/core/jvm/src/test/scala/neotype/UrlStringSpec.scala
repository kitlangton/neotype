package neotype

import zio.test.*

import scala.compiletime.testing.*

object UrlStringSpec extends ZIOSpecDefault:
  val spec = suiteAll("UrlStringSpec") {

    test("url success") {
      val res = UrlStringNewtype("http://example.com")
      assertTrue(res.unwrap == "http://example.com")
    }

    test("url failure") {
      val res = typeCheckErrors(""" UrlStringNewtype("abc") """).head
      assertTrue(res.message contains "Must be a valid URL")
    }

    test("common.UrlString success") {
      val res = neotype.common.UrlString("http://example.com")
      assertTrue(res == "http://example.com")
    }

    test("common.UrlString failure") {
      val res = typeCheckErrors(""" neotype.common.UrlString("abc") """).head
      assertTrue(res.message contains "Must be a valid URL")
    }

    test("url make success") {
      val res = UrlStringNewtype.make("http://example.com")
      assertTrue(res.map(_.unwrap) == Right("http://example.com"))
    }

    test("url make failure") {
      val res = UrlStringNewtype.make("abc")
      assertTrue(res == Left("Must be a valid URL"))
    }

    test("common.UrlString make success") {
      val res = neotype.common.UrlString.make("http://example.com")
      assertTrue(res.contains("http://example.com"))
    }

    test("common.UrlString make failure") {
      val res = neotype.common.UrlString.make("abc")
      assertTrue(res == Left("Must be a valid URL"))
    }
  }
