package comptime

import scala.quoted.*

private[comptime] object ScalaAstBridgePos:
  private val ContextLines = 1 // Number of lines before/after to include

  def extractPos[Q <: Quotes](using quotes: Q)(term: quotes.reflect.Term): Option[SourcePos] =
    val pos = term.pos
    if pos.sourceFile.path == null then None
    else
      try
        val file     = pos.sourceFile.path
        val line     = pos.startLine + 1 // 1-indexed
        val startCol = pos.startColumn
        val endCol   = pos.endColumn

        // Try to get source lines
        val (lineContent, contextBefore, contextAfter) =
          try
            pos.sourceFile.content match
              case Some(content) =>
                val lines   = content.split('\n')
                val lineIdx = pos.startLine
                if lineIdx >= 0 && lineIdx < lines.length then
                  val mainLine = lines(lineIdx)
                  // Get context lines before
                  val before = (math.max(0, lineIdx - ContextLines) until lineIdx)
                    .map(i => (i + 1, lines(i))) // 1-indexed line numbers
                    .toList
                  // Get context lines after
                  val after = ((lineIdx + 1) to math.min(lines.length - 1, lineIdx + ContextLines))
                    .map(i => (i + 1, lines(i)))
                    .toList
                  (Some(mainLine), before, after)
                else (None, Nil, Nil)
              case None => (None, Nil, Nil)
          catch case _: Throwable => (None, Nil, Nil)

        Some(SourcePos(file, line, startCol, endCol, lineContent.getOrElse(""), contextBefore, contextAfter))
      catch case _: Throwable => None
