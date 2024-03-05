package neotype

object CustomFailureNewtype extends Newtype[String]:
  override inline def validate(value: String) =
    if value == "secret string" then true else "Must be the secret string!"

object EqualityParsingNewtype extends Newtype[String]:
  override inline def validate(value: String): Boolean =
    val long: Long       = 5L
    val int: Int         = 10
    val char: Char       = 'a'
    val short: Short     = 1
    val byte: Byte       = 2
    val float: Float     = 3.0f
    val double: Double   = 4.0
    val boolean: Boolean = true
    long == 5L && int == 10 && char == 'a' && short == 1 && byte == 2 && float == 3.0f && double == 4.0 && boolean == true

object LessThanParsingNewtype extends Newtype[String]:
  override inline def validate(value: String): Boolean =
    val long: Long     = 5L
    val int: Int       = 10
    val char: Char     = 'a'
    val short: Short   = 1
    val byte: Byte     = 2
    val float: Float   = 3.0f
    val double: Double = 4.0
    long < 6L && int < 11 && (char < 'b' && 2 < long) && byte < 3 && float < 4.0f && double < 5.0 && short < 2
