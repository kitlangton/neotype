package comptime

/** Exception thrown to signal an explicit compile-time error.
  *
  * When thrown during compile-time evaluation, this becomes a compile error
  * with the provided message. Use via the `comptimeError` helper:
  *
  * {{{
  * comptime {
  *   if condition then value
  *   else comptimeError("Condition not met")
  * }
  * }}}
  */
case class ComptimeAbort(message: String) extends Exception(message)
