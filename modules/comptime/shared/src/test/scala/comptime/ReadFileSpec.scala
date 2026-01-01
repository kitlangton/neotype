package comptime

import zio.test.*

import java.nio.charset.StandardCharsets
import scala.compiletime.testing.*

object ReadFileSpec extends ZIOSpecDefault:
  private val expectedText =
    "alpha\nbeta\ngamma\n"

  private val expectedBytes = expectedText.getBytes(StandardCharsets.UTF_8)
  private val missingPath   = "modules/comptime/shared/src/test/scala/comptime/fixtures/missing.txt"

  val spec =
    suite("ReadFileSpec")(
      test("readFile reads text relative to the source file") {
        assertTrue(
          comptime(readFile("fixtures/readfile.txt")) == expectedText
        )
      },
      test("readFile accepts an explicit encoding") {
        assertTrue(
          comptime(readFile("fixtures/readfile.txt", "UTF-8")) == expectedText
        )
      },
      test("readFileBytes returns bytes") {
        val bytes = comptime(readFileBytes("fixtures/readfile.txt"))
        assertTrue(bytes.sameElements(expectedBytes))
      },
      test("readFile missing file shows a helpful error") {
        val errors = typeCheckErrors("""
          comptime {
            readFile("modules/comptime/shared/src/test/scala/comptime/fixtures/missing.txt")
          }
        """)
        assertTrue(
          errors.nonEmpty,
          errors.exists(_.message.contains("file not found")),
          errors.exists(_.message.contains(missingPath)),
          errors.exists(_.message.contains("paths are resolved relative to the source file"))
        )
      },
      test("readFile unsupported encoding reports an error") {
        val errors = typeCheckErrors("""
          comptime {
            readFile("modules/comptime/shared/src/test/scala/comptime/fixtures/readfile.txt", "NOPE-ENCODING")
          }
        """)
        assertTrue(
          errors.nonEmpty,
          errors.exists(_.message.contains("unsupported encoding")),
          errors.exists(_.message.contains("NOPE-ENCODING"))
        )
      }
    )
