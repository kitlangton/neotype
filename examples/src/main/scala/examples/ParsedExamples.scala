package examples

// ═══════════════════════════════════════════════════════════════════════════════
// PARSED EXAMPLES - Compile-time string parsing
// ═══════════════════════════════════════════════════════════════════════════════
//
// `Parsed[A]` lets you parse strings into structured types at compile time.
// Unlike Subtype validation (which checks a condition), Parsed transforms
// the input string into a completely different type.
//
// - At compile time: `Parser("literal")` parses and inlines the result
// - At runtime: `Parser.make(str)` returns Either[String, A]
//
// NOTE: Parsed type definitions are in ParsedTypes.scala - they must be in a
// separate file from usage for the comptime macro to resolve them correctly.
// ═══════════════════════════════════════════════════════════════════════════════

object SemVerExamples:
  // Compile-time parsing - invalid strings fail to compile
  val v1 = SemVer("1.0.0")
  val v2 = SemVer("2.1.3")
  val v3 = SemVer("10.20.30")

  // Uncomment to see compile error:
  // val bad = SemVer("1.2")        // ✗ Invalid semver format
  // val bad = SemVer("1.2.three")  // ✗ Invalid semver numbers

  // Runtime parsing - returns Either
  val runtime: Either[String, SemVer] =
    SemVer.make(sys.env.getOrElse("APP_VERSION", "0.0.1"))

object IPv4Examples:
  val localhost = IPv4("127.0.0.1")
  val google    = IPv4("8.8.8.8")
  val private_  = IPv4("192.168.1.1")

  // Uncomment to see compile errors:
  // val bad = IPv4("256.0.0.1")   // ✗ Octets must be 0-255
  // val bad = IPv4("1.2.3")       // ✗ Invalid IP format

object RGBExamples:
  val red   = RGB("#ff0000")
  val green = RGB("#00ff00")
  val coral = RGB("#ff7f50")

  // Access components
  val r = red.red   // 255
  val g = red.green // 0
  val b = red.blue  // 0

  // Uncomment to see compile errors:
  // val bad = RGB("ff0000")   // ✗ Must start with #
  // val bad = RGB("#gggggg")  // ✗ Invalid hex digits

object SimpleDurationExamples:
  val shortBreak = SimpleDuration("15m")
  val movie      = SimpleDuration("2h30m")
  val precise    = SimpleDuration("1h45m30s")

  // Uncomment to see compile errors:
  // val bad = SimpleDuration("90m")  // ✗ Minutes must be < 60
