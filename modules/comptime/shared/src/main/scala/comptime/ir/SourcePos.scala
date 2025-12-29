package comptime

/** Source position for error reporting.
  *
  * @param file
  *   Source file path
  * @param line
  *   Line number (1-indexed)
  * @param startCol
  *   Start column (0-indexed)
  * @param endCol
  *   End column (0-indexed, exclusive)
  * @param lineContent
  *   The full source line text
  * @param contextBefore
  *   Lines before the error line
  * @param contextAfter
  *   Lines after the error line
  */
final case class SourcePos(
    file: String,
    line: Int,
    startCol: Int,
    endCol: Int,
    lineContent: String,
    contextBefore: List[(Int, String)] = Nil, // (lineNum, content)
    contextAfter: List[(Int, String)] = Nil
)
