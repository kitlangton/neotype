package neotype

import zio.test.*

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Try

object TestUtils:
  /** Strip ANSI escape codes from a string for stable snapshot comparisons */
  def stripAnsi(text: String): String =
    text.replaceAll("\u001B\\[[0-9;]*m", "")

  /** Normalize line endings and trim for cross-platform consistency */
  def normalize(text: String): String =
    stripAnsi(text).replaceAll("\r\n", "\n").trim

  private val defaultSnapshotDir: Path =
    Paths.get("modules/core/jvm/src/test/scala/neotype/__snapshots__")

  private def shouldUpdateSnapshots: Boolean =
    sys.env.get("UPDATE_SNAPSHOTS").exists(_.nonEmpty) ||
      sys.props.get("updateSnapshots").exists(_.nonEmpty)

  /** Assert that actual content matches a snapshot file. Set
    * UPDATE_SNAPSHOTS=true env var or -DupdateSnapshots=true to update
    * snapshots. Missing snapshots are auto-created on first run.
    */
  def assertSnapshot(actual: String, name: String): TestResult =
    assertSnapshot(actual, name, defaultSnapshotDir)

  /** Assert that actual content matches a snapshot file in a custom directory.
    */
  def assertSnapshot(actual: String, name: String, snapshotDir: Path): TestResult =
    val snapshotPath = snapshotDir.resolve(s"$name.snap")
    val normalized   = normalize(actual)

    Try(Files.readString(snapshotPath)).toOption match
      case Some(expected) =>
        val exp = expected.trim
        if normalized == exp then assertTrue(true)
        else if shouldUpdateSnapshots then
          Files.writeString(snapshotPath, normalized)
          assertTrue(true)
        else
          // Provide a helpful diff message
          assertTrue(normalized == exp)
      case None =>
        // Auto-create missing snapshots
        Files.createDirectories(snapshotDir)
        Files.writeString(snapshotPath, normalized)
        System.err.println(s"[snapshot] Created: $snapshotPath")
        assertTrue(true)
