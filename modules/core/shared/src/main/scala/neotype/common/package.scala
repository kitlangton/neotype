package neotype.common

import neotype.*

// ────────────────────────────────────────────────────────────────────────────
// String types
// ────────────────────────────────────────────────────────────────────────────

type NonEmptyString = NonEmptyString.Type
object NonEmptyString extends Subtype[String]:
  override inline def validate(input: String): Boolean | String =
    if input.isBlank then "NonEmptyString cannot be empty" else true

type UriString = UriString.Type
object UriString extends Subtype[String]:
  override inline def validate(input: String): Boolean | String =
    if scala.util.Try(new java.net.URI(input)).isFailure then "Must be a valid URI" else true

type UuidString = UuidString.Type
object UuidString extends Subtype[String]:
  override inline def validate(input: String): Boolean | String =
    if !scala.util.Try(java.util.UUID.fromString(input).toString).toOption.contains(input.toLowerCase) then
      "Must be a valid UUID"
    else true

type HexString = HexString.Type
object HexString extends Subtype[String]:
  override inline def validate(input: String): Boolean | String =
    if input.isEmpty then "HexString cannot be empty"
    else if !input.forall(c => c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) then
      "Must contain only hexadecimal characters (0-9, a-f, A-F)"
    else true

type EmailString = EmailString.Type
object EmailString extends Subtype[String]:
  // Basic email validation: non-empty local part, @, non-empty domain with at least one dot
  override inline def validate(input: String): Boolean | String =
    input.split("@", 2) match
      case Array(local, domain)
          if local.nonEmpty && domain.contains(".") && !domain.startsWith(".") && !domain.endsWith(".") =>
        true
      case _ => "Must be a valid email address"

// ────────────────────────────────────────────────────────────────────────────
// Numeric types - Int
// ────────────────────────────────────────────────────────────────────────────

type PositiveInt = PositiveInt.Type
object PositiveInt extends Subtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input <= 0 then "Must be positive" else true

type NonNegativeInt = NonNegativeInt.Type
object NonNegativeInt extends Subtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input < 0 then "Must be non-negative" else true

type NegativeInt = NegativeInt.Type
object NegativeInt extends Subtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input >= 0 then "Must be negative" else true

// ────────────────────────────────────────────────────────────────────────────
// Numeric types - Long
// ────────────────────────────────────────────────────────────────────────────

type PositiveLong = PositiveLong.Type
object PositiveLong extends Subtype[Long]:
  override inline def validate(input: Long): Boolean | String =
    if input <= 0 then "Must be positive" else true

type NonNegativeLong = NonNegativeLong.Type
object NonNegativeLong extends Subtype[Long]:
  override inline def validate(input: Long): Boolean | String =
    if input < 0 then "Must be non-negative" else true

type NegativeLong = NegativeLong.Type
object NegativeLong extends Subtype[Long]:
  override inline def validate(input: Long): Boolean | String =
    if input >= 0 then "Must be negative" else true

// ────────────────────────────────────────────────────────────────────────────
// Numeric types - Double
// ────────────────────────────────────────────────────────────────────────────

type PositiveDouble = PositiveDouble.Type
object PositiveDouble extends Subtype[Double]:
  override inline def validate(input: Double): Boolean | String =
    if input <= 0 then "Must be positive" else true

type NonNegativeDouble = NonNegativeDouble.Type
object NonNegativeDouble extends Subtype[Double]:
  override inline def validate(input: Double): Boolean | String =
    if input < 0 then "Must be non-negative" else true

type Percentage = Percentage.Type
object Percentage extends Subtype[Double]:
  override inline def validate(input: Double): Boolean | String =
    if input < 0.0 || input > 100.0 then "Must be between 0 and 100" else true

type UnitInterval = UnitInterval.Type
object UnitInterval extends Subtype[Double]:
  override inline def validate(input: Double): Boolean | String =
    if input < 0.0 || input > 1.0 then "Must be between 0.0 and 1.0" else true

// ────────────────────────────────────────────────────────────────────────────
// Network types
// ────────────────────────────────────────────────────────────────────────────

type PortNumber = PortNumber.Type
object PortNumber extends Subtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input < 0 || input > 65535 then "Must be a valid port number (0-65535)" else true
