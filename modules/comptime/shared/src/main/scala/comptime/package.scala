package object comptime:
  import scala.quoted.*
  import _root_.comptime.MacroEntry

  /** Evaluate an expression at compile time.
    *
    * All failures become compile errors:
    *   - If the expression cannot be compiled (unsupported construct), compile
    *     error
    *   - If evaluation throws an exception, compile error
    *
    * Use `comptimeError` for explicit compile-time errors.
    */
  inline def comptime[A](inline expr: A): A =
    ${ MacroEntry.comptimeImpl('expr) }

  /** Throw a compile-time error with a custom message.
    *
    * {{{
    * comptime {
    *   val n = parseNumber(s)
    *   if n > 0 then n
    *   else comptimeError(s"Expected positive, got $n")
    * }
    * }}}
    */
  inline def comptimeError(message: String): Nothing =
    throw ComptimeAbort(message)

  // TODO: Add compile-time file reading:
  //
  //   inline def readFile(inline path: String): String
  //   inline def readFileBytes(inline path: String): IArray[Byte]
  //   inline def readResource(inline path: String): String  // classpath resource
  //
  // Implementation notes:
  //   - Resolve path relative to source file (use Position.sourceFile)
  //   - Read file content during macro expansion
  //   - Return as compile-time constant String
  //   - The result can then be transformed with comptime-capable ops:
  //       comptime { readFile("data.csv").split("\n").map(_.split(",")) }
  //
  // Dangers to document:
  //   - Non-determinism: file changes don't trigger recompile
  //   - Path resolution: relative to source file or project root?
  //   - Security: limit to project directory, warn on absolute paths
  //   - Prefer readResource for classpath resources (more portable)
