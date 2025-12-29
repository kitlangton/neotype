package examples

import comptime.*

// ═══════════════════════════════════════════════════════════════════════════════
// COMPTIME EXAMPLES
// ═══════════════════════════════════════════════════════════════════════════════
//
// This file demonstrates creative uses of the `comptime` macro beyond simple
// parsing. The key insight: anything expressible with comptime-supported
// operations can be pre-evaluated at compile time, resulting in zero runtime cost.
//
// ═══════════════════════════════════════════════════════════════════════════════

// ─────────────────────────────────────────────────────────────────────────────
// 1. PRE-COMPUTED LOOKUP TABLES
// ─────────────────────────────────────────────────────────────────────────────
//
// Expensive computations are evaluated ONCE at compile time and inlined as
// literal values. The bytecode contains the pre-computed results directly.
//

/** Pre-computed constants - zero runtime cost, values inlined in bytecode. */
object LookupTables:

  /** All prime numbers under 100, computed at compile time.
    *
    * The resulting bytecode literally contains:
    * `List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)`
    */
  val primesUnder100: List[Int] = comptime {
    (2 to 100).toList.filter(n => (2 until n).forall(n % _ != 0))
  }

  /** Factorial lookup table for 0! through 12! (max that fits in Int).
    *
    * Instead of computing factorials at runtime, we pre-compute them.
    */
  val factorials: List[(Int, Int)] = comptime {
    (0 to 12).toList.map(n => (n, (1 to n).product))
  }

// Usage examples:
object LookupTableExamples:
  // These are just list lookups at runtime - the computation already happened
  val isPrime = LookupTables.primesUnder100.contains(97)          // true
  val fact5   = LookupTables.factorials.find(_._1 == 5).map(_._2) // Some(120)

// ─────────────────────────────────────────────────────────────────────────────
// 2. STATIC ASSERTIONS
// ─────────────────────────────────────────────────────────────────────────────
//
// Like C's `static_assert` - fail compilation if an invariant is violated.
//
// THE KEY INSIGHT: Use `inline val` to define constants, then staticAssert
// validates relationships between them. When someone changes one constant
// without considering the others, compilation fails immediately.
//
// This is like compile-time unit tests for your configuration!
//

/** Assert a condition at compile time. Fails compilation if condition is false.
  *
  * The condition must use only:
  *   - Literals (numbers, strings, booleans)
  *   - `inline val` constants
  *   - Supported stdlib operations
  *
  * Use `inline val` to factor out constants, then validate their relationships.
  */
inline def staticAssert(inline cond: Boolean, inline msg: String): Unit =
  comptime {
    if !cond then comptimeError(msg)
  }

// ═══════════════════════════════════════════════════════════════════════════
// REAL-WORLD EXAMPLE: Buffer Protocol Configuration
// ═══════════════════════════════════════════════════════════════════════════
//
// Imagine these constants are spread across your codebase, maintained by
// different people. Static assertions catch broken invariants at compile time!
//
object BufferConfig:
  // ── Protocol constants (maybe from a spec document) ──
  inline val HEADER_SIZE = 12   // bytes: magic(4) + version(2) + length(2) + flags(4)
  inline val MAX_PAYLOAD = 1500 // bytes: standard MTU minus headers
  inline val PACKET_SIZE = 1512 // total packet size

  // ── Buffer sizing (maybe tuned by ops team) ──
  inline val RING_BUFFER_SIZE = 4096 // must be power of 2 for efficient modulo
  inline val MAX_PACKETS      = 2    // packets to buffer

  // ═══ INVARIANT CHECKS ═══
  // These catch bugs when ANY of the above constants change!

  // Protocol math must add up
  staticAssert(
    HEADER_SIZE + MAX_PAYLOAD == PACKET_SIZE,
    "Protocol error: header + payload must equal packet size"
  )

  // Ring buffer must be power of 2 (for fast & instead of %)
  staticAssert(
    (RING_BUFFER_SIZE & (RING_BUFFER_SIZE - 1)) == 0,
    "Ring buffer size must be power of 2"
  )

  // Buffer must fit the packets we want to hold
  staticAssert(
    RING_BUFFER_SIZE >= MAX_PACKETS * PACKET_SIZE,
    "Ring buffer too small for MAX_PACKETS"
  )

  // ── Uncomment to see failures ──
  // staticAssert(10 + 1500 == 1512, "header size changed but packet size didn't")
  // staticAssert((3000 & 2999) == 0, "3000 is not power of 2")

// ═══════════════════════════════════════════════════════════════════════════
// REAL-WORLD EXAMPLE: Connection Pool Settings
// ═══════════════════════════════════════════════════════════════════════════
object PoolConfig:
  inline val MIN_CONNECTIONS     = 5
  inline val MAX_CONNECTIONS     = 20
  inline val IDLE_TIMEOUT_SEC    = 300 // 5 minutes
  inline val CONNECT_TIMEOUT_SEC = 30
  inline val QUEUE_CAPACITY      = 100

  // Pool size sanity
  staticAssert(MIN_CONNECTIONS > 0, "Need at least 1 connection")
  staticAssert(MAX_CONNECTIONS >= MIN_CONNECTIONS, "Max must be >= min")
  staticAssert(MAX_CONNECTIONS <= 100, "Too many connections will exhaust DB")

  // Timeout relationships
  staticAssert(
    IDLE_TIMEOUT_SEC > CONNECT_TIMEOUT_SEC,
    "Idle timeout should exceed connect timeout"
  )

  // Queue should handle burst when pool exhausted
  staticAssert(
    QUEUE_CAPACITY >= MAX_CONNECTIONS * 2,
    "Queue too small to handle connection burst"
  )

  // ── Uncomment to see failures ──
  // staticAssert(0 > 0, "min connections is 0")
  // staticAssert(5 >= 20, "min > max")
  // staticAssert(30 > 300, "connect timeout > idle timeout")

// ═══════════════════════════════════════════════════════════════════════════
// REAL-WORLD EXAMPLE: API Rate Limiting
// ═══════════════════════════════════════════════════════════════════════════
object RateLimitConfig:
  inline val REQUESTS_PER_SECOND = 100
  inline val BURST_CAPACITY      = 50
  inline val WINDOW_SIZE_MS      = 1000
  inline val MAX_RETRY_ATTEMPTS  = 3
  inline val RETRY_BASE_DELAY_MS = 100

  // Burst can't exceed sustained rate
  staticAssert(
    BURST_CAPACITY <= REQUESTS_PER_SECOND,
    "Burst capacity exceeds sustained rate"
  )

  // Window must be reasonable
  staticAssert(
    WINDOW_SIZE_MS >= 100 && WINDOW_SIZE_MS <= 60000,
    "Window size must be 100ms - 60s"
  )

  // Retry backoff won't exceed window
  staticAssert(
    RETRY_BASE_DELAY_MS * (1 << MAX_RETRY_ATTEMPTS) <= WINDOW_SIZE_MS,
    "Max retry delay exceeds rate limit window"
  )

// ═══════════════════════════════════════════════════════════════════════════
// REAL-WORLD EXAMPLE: Video Encoding Parameters
// ═══════════════════════════════════════════════════════════════════════════
object VideoConfig:
  inline val WIDTH             = 1280 // 720p - both dimensions divisible by 16
  inline val HEIGHT            = 720
  inline val FRAME_RATE        = 30
  inline val KEYFRAME_INTERVAL = 60   // frames between keyframes

  // Dimensions must be divisible by macroblock size (16 for H.264)
  staticAssert(WIDTH  % 16 == 0, "Width must be divisible by 16 (H.264 macroblock)")
  staticAssert(HEIGHT % 16 == 0, "Height must be divisible by 16 (H.264 macroblock)")

  // Keyframe interval should be multiple of frame rate (clean seconds)
  staticAssert(
    KEYFRAME_INTERVAL % FRAME_RATE == 0,
    "Keyframe interval should be multiple of frame rate"
  )

  // Sanity bounds
  staticAssert(WIDTH * HEIGHT <= 4096 * 2160, "Exceeds 4K resolution")
  staticAssert(FRAME_RATE >= 24 && FRAME_RATE <= 120, "Frame rate out of range")

  // ── Uncomment to see failures ──
  // staticAssert(1919 % 16 == 0, "1919 not divisible by 16")
  // staticAssert(1080 % 16 == 0, "1080p height fails! Use 720p or 1088")
  // staticAssert(45 % 30 == 0, "45 not divisible by 30")

// ═══════════════════════════════════════════════════════════════════════════
// REAL-WORLD EXAMPLE: Feature Flag Dependencies
// ═══════════════════════════════════════════════════════════════════════════
object FeatureFlags:
  inline val DARK_MODE_ENABLED  = true
  inline val THEMES_ENABLED     = true  // Dark mode requires themes
  inline val NEW_EDITOR_ENABLED = true
  inline val AUTOSAVE_ENABLED   = true  // Autosave requires new editor
  inline val COLLAB_ENABLED     = false
  inline val COMMENTS_ENABLED   = false // Comments require collab

  // Dependency: dark mode requires themes infrastructure
  staticAssert(
    !DARK_MODE_ENABLED || THEMES_ENABLED,
    "Dark mode requires themes to be enabled"
  )

  // Dependency: autosave requires new editor
  staticAssert(
    !AUTOSAVE_ENABLED || NEW_EDITOR_ENABLED,
    "Autosave requires new editor"
  )

  // Dependency: comments require collab
  staticAssert(
    !COMMENTS_ENABLED || COLLAB_ENABLED,
    "Comments require collaboration mode"
  )

  // ── Uncomment to see failures ──
  // inline val BAD_DARK_MODE = true
  // inline val BAD_THEMES = false
  // staticAssert(!BAD_DARK_MODE || BAD_THEMES, "dark mode without themes!")

// ─────────────────────────────────────────────────────────────────────────────
// 3. DURATION LITERALS
// ─────────────────────────────────────────────────────────────────────────────
//
// Parse simple duration strings like "30s", "5m", or "2h" at compile time
// into java.time.Duration. Invalid formats fail to compile.
//

/** Parse a simple duration string at compile time.
  *
  * Supported format: a single unit like "30s", "5m", or "2h"
  *
  * Examples:
  *   - `simpleDuration("30s")` → Duration of 30 seconds
  *   - `simpleDuration("5m")` → Duration of 5 minutes
  *   - `simpleDuration("2h")` → Duration of 2 hours
  */
inline def simpleDuration(inline s: String): java.time.Duration = comptime {
  val len = s.length
  if len < 2 then comptimeError(s"Invalid duration: '$s'")

  val unit   = s.charAt(len - 1)
  val numStr = s.substring(0, len - 1)
  val n      = numStr.toLong // Use Long for Duration methods

  unit match
    case 'h' => java.time.Duration.ofHours(n)
    case 'm' => java.time.Duration.ofMinutes(n)
    case 's' => java.time.Duration.ofSeconds(n)
    case _   => comptimeError(s"Unknown duration unit: '$unit'. Use h, m, or s")
}

// Usage examples:
object DurationExamples:
  val timeout     = simpleDuration("30s") // Duration.ofSeconds(30)
  val longTimeout = simpleDuration("90m") // Duration.ofMinutes(90)
  val retryDelay  = simpleDuration("5s")  // Duration.ofSeconds(5)
  val hourWait    = simpleDuration("2h")  // Duration.ofHours(2)

  // Uncomment to see compile error:
  // val bad = simpleDuration("invalid")

// ─────────────────────────────────────────────────────────────────────────────
// 4. VALIDATED REGEX (at compile time, used at runtime)
// ─────────────────────────────────────────────────────────────────────────────
//
// Validate regex patterns at compile time - if the pattern is invalid,
// compilation fails. The regex is created at runtime but we know it's valid.
//

/** Create a regex pattern that's validated at compile time.
  *
  * The pattern string is validated during compilation, ensuring invalid
  * patterns fail early. Returns the pattern string for runtime regex creation.
  */
inline def validatedPattern(inline pattern: String): String = comptime {
  // This validates the regex at compile time - throws PatternSyntaxException if invalid
  pattern.r
  // Return the pattern string (regex object can't be lifted)
  pattern
}

// Usage examples:
object RegexExamples:
  // Patterns validated at compile time, regex created at runtime
  val emailPattern = validatedPattern("""[\w.]+@[\w.]+\.\w+""").r
  val phonePattern = validatedPattern("""\d{3}-\d{3}-\d{4}""").r
  val isoDate      = validatedPattern("""\d{4}-\d{2}-\d{2}""").r

  // Uncomment to see compile error (unclosed group):
  // val broken = validatedPattern("""(unclosed""").r

  // Usage is just normal Regex operations
  val hasEmail = emailPattern.matches("test@example.com")

// ─────────────────────────────────────────────────────────────────────────────
// 5. MANUAL PARSING WITH COMPTIME
// ─────────────────────────────────────────────────────────────────────────────
//
// You can use `comptime` directly to parse strings at compile time.
// This shows the manual pattern - for a cleaner API, see `Parsed` trait
// in ParsedExamples.scala which factors out this boilerplate.
//

/** Chess square notation parsed at compile time (manual comptime approach). */
final case class ChessSquare(file: Char, rank: Int):
  override def toString: String = s"$file$rank"

object ChessSquare:
  private inline def doParse(notation: String): Either[String, ChessSquare] =
    if notation.length != 2 then Left(s"Invalid: '$notation'. Expected e.g. 'e4'")
    else
      val file = notation.charAt(0)
      val rank = notation.charAt(1)
      if file < 'a' || file > 'h' then Left(s"File must be a-h, got '$file'")
      else if rank < '1' || rank > '8' then Left(s"Rank must be 1-8, got '$rank'")
      else Right(ChessSquare(file, rank.toInt - 48))

  /** Compile-time parsing - invalid notation fails to compile. */
  inline def apply(inline notation: String): ChessSquare = comptime {
    doParse(notation) match
      case Right(sq) => sq
      case Left(err) => comptimeError(err)
  }

  /** Runtime parsing - for user input. */
  def parse(notation: String): Either[String, ChessSquare] = doParse(notation)

object ManualParsingExample:
  val e4 = ChessSquare("e4") // ✓ Compiles to ChessSquare('e', 4)
  val a1 = ChessSquare("a1") // ✓ Compiles

  // Uncomment to see compile error:
  // val bad = ChessSquare("z9") // ✗ File must be a-h

  // Runtime parsing for user input
  def fromUser(input: String) = ChessSquare.parse(input) // Either[String, ChessSquare]
