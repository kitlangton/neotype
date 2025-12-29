package neotype

import zio.test.*

import scala.compiletime.testing.*

import TestUtils.*

object ErrorMessageSnapshotSpec extends ZIOSpecDefault:
  val spec = suiteAll("ErrorMessageSnapshotSpec") {
    test("validation failure - positive int") {
      val res = typeCheckErrors("""PositiveIntNewtype(-1)""").head
      assertSnapshot(res.message, "validation_failure_positive_int")
    }

    test("validation failure - custom message") {
      val res = typeCheckErrors("""CustomFailureNewtype("hello")""").head
      assertSnapshot(res.message, "validation_failure_custom_message")
    }

    test("validation failure - non empty string") {
      val res = typeCheckErrors("""NonEmptyStringNewtype("")""").head
      assertSnapshot(res.message, "validation_failure_non_empty")
    }

    test("validation failure - uri") {
      val res = typeCheckErrors("""UriStringNewtype("hello world")""").head
      assertSnapshot(res.message, "validation_failure_uri")
    }

    test("validation failure - uuid") {
      val res = typeCheckErrors("""UuidStringNewtype("not-a-uuid")""").head
      assertSnapshot(res.message, "validation_failure_uuid")
    }
  }
