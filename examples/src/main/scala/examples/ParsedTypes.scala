package examples

import neotype.*

// ═══════════════════════════════════════════════════════════════════════════════
// PARSED TYPE DEFINITIONS
// ═══════════════════════════════════════════════════════════════════════════════
// These must be in a separate file from their usage for comptime macro resolution.

// ─────────────────────────────────────────────────────────────────────────────
// SEMANTIC VERSION - Parse "major.minor.patch" format
// ─────────────────────────────────────────────────────────────────────────────

final case class SemVer(major: Int, minor: Int, patch: Int)

object SemVer extends Parsed[SemVer]:
  override inline def parse(inline input: String): Either[String, SemVer] =
    input.split("\\.").toList match
      case List(maj, min, pat) =>
        (maj.toIntOption, min.toIntOption, pat.toIntOption) match
          case (Some(a), Some(b), Some(c)) => Right(SemVer(a, b, c))
          case _                           => Left(s"Invalid semver numbers: $input")
      case _ =>
        Left(s"Invalid semver format (expected x.y.z): $input")

  def apply(major: Int, minor: Int, patch: Int): SemVer =
    new SemVer(major, minor, patch)

// ─────────────────────────────────────────────────────────────────────────────
// IP ADDRESS - Parse "a.b.c.d" format with octet validation
// ─────────────────────────────────────────────────────────────────────────────

final case class IPv4(a: Int, b: Int, c: Int, d: Int):
  override def toString: String = s"$a.$b.$c.$d"

object IPv4 extends Parsed[IPv4]:
  override inline def parse(inline input: String): Either[String, IPv4] =
    input.split("\\.").toList match
      case List(a, b, c, d) =>
        val parts = List(a, b, c, d).map(_.toIntOption)
        if parts.exists(_.isEmpty) then Left(s"Invalid IP octets: $input")
        else
          val nums = parts.flatten
          if nums.exists(n => n < 0 || n > 255) then Left(s"Octets must be 0-255: $input")
          else Right(IPv4(nums(0), nums(1), nums(2), nums(3)))
      case _ =>
        Left(s"Invalid IP format (expected a.b.c.d): $input")

  def apply(a: Int, b: Int, c: Int, d: Int): IPv4 = new IPv4(a, b, c, d)

// ─────────────────────────────────────────────────────────────────────────────
// HEX COLOR - Parse "#RRGGBB" into RGB components
// ─────────────────────────────────────────────────────────────────────────────

final case class RGB(red: Int, green: Int, blue: Int):
  def toHex: String = f"#$red%02x$green%02x$blue%02x"

object RGB extends Parsed[RGB]:
  // Helper to parse a single hex digit - must be inline so it expands in parse
  private inline def hexDigit(c: Char): Int =
    if c >= '0' && c <= '9' then c - '0'
    else if c >= 'a' && c <= 'f' then c - 'a' + 10
    else if c >= 'A' && c <= 'F' then c - 'A' + 10
    else -1

  override inline def parse(inline input: String): Either[String, RGB] =
    if !input.startsWith("#") then Left("Color must start with #")
    else if input.length != 7 then Left("Color must be #RRGGBB format")
    else
      val hex    = input.drop(1)
      val digits = hex.map(c => hexDigit(c)) // explicit lambda, not eta-expansion
      if digits.exists(_ < 0) then Left("Invalid hex digits")
      else
        val r = digits(0) * 16 + digits(1)
        val g = digits(2) * 16 + digits(3)
        val b = digits(4) * 16 + digits(5)
        Right(RGB(r, g, b))

  def apply(r: Int, g: Int, b: Int): RGB = new RGB(r, g, b)

// ─────────────────────────────────────────────────────────────────────────────
// TIME DURATION - Parse "1h30m15s" format (simplified)
// ─────────────────────────────────────────────────────────────────────────────

final case class SimpleDuration(hours: Int, minutes: Int, seconds: Int):
  def totalSeconds: Int = hours * 3600 + minutes * 60 + seconds

object SimpleDuration extends Parsed[SimpleDuration]:
  override inline def parse(inline input: String): Either[String, SimpleDuration] =
    // Use regex for cleaner parsing (supported by comptime)
    val pattern = """(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s)?""".r
    input match
      case pattern(h, m, s) =>
        val hours   = Option(h).flatMap(_.toIntOption).getOrElse(0)
        val minutes = Option(m).flatMap(_.toIntOption).getOrElse(0)
        val seconds = Option(s).flatMap(_.toIntOption).getOrElse(0)
        if minutes >= 60 || seconds >= 60 then Left("Minutes and seconds must be < 60")
        else Right(SimpleDuration(hours, minutes, seconds))
      case _ =>
        Left(s"Invalid duration format: $input")

  def apply(h: Int, m: Int, s: Int): SimpleDuration = new SimpleDuration(h, m, s)
