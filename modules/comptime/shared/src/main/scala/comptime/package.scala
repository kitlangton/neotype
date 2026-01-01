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

  /** Read a file as a String during compile-time evaluation.
    *
    * Paths are resolved relative to the source file containing the call.
    * Uses UTF-8 by default.
    */
  def readFile(path: String, encoding: String = "UTF-8"): String =
    throw new RuntimeException("comptime.readFile can only be used inside comptime(...)")

  /** Read a file as bytes during compile-time evaluation.
    *
    * Paths are resolved relative to the source file containing the call.
    */
  def readFileBytes(path: String): Array[Byte] =
    throw new RuntimeException("comptime.readFileBytes can only be used inside comptime(...)")
