package comptime

import neotype.TestUtils
import zio.test.*

import java.nio.file.Paths
import scala.compiletime.testing.*

object ReadFileSnapshotSpec extends ZIOSpecDefault:
  private val snapshotDir =
    Paths.get("modules/comptime/jvm/src/test/scala/comptime/__snapshots__")

  private def normalizePaths(message: String): String =
    val cwd = Paths.get("").toAbsolutePath.normalize().toString
    message.replace(cwd, "<cwd>")

  private def messageFor(errors: List[Error]): String =
    errors.map(_.message).find(_.contains("comptime.readFile")).getOrElse(errors.head.message)

  val spec =
    suite("ReadFileSnapshotSpec")(
      test("missing file error snapshot") {
        val errors = typeCheckErrors("""
          comptime {
            readFile("modules/comptime/shared/src/test/scala/comptime/fixtures/missing.txt")
          }
        """)
        assertTrue(errors.nonEmpty) &&
        TestUtils.assertSnapshot(
          normalizePaths(messageFor(errors)),
          "comptime_readfile_missing",
          snapshotDir
        )
      },
      test("unsupported encoding error snapshot") {
        val errors = typeCheckErrors("""
          comptime {
            readFile("modules/comptime/shared/src/test/scala/comptime/fixtures/readfile.txt", "NOPE-ENCODING")
          }
        """)
        assertTrue(errors.nonEmpty) &&
        TestUtils.assertSnapshot(
          normalizePaths(messageFor(errors)),
          "comptime_readfile_bad_encoding",
          snapshotDir
        )
      }
    )
