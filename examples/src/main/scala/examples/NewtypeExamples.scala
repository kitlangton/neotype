package examples

import neotype.*

// ═══════════════════════════════════════════════════════════════════════════════
// NEOTYPE: A Story in Five Parts
// ═══════════════════════════════════════════════════════════════════════════════

// ┌─────────────────────────────────────────────────────────────────────────────┐
// │ PART 1: THE PROBLEM                                                         │
// │                                                                             │
// │ Primitive types are dangerous. A UserId and OrderId are both Long,          │
// │ but mixing them up is a bug. The compiler can't help you.                   │
// └─────────────────────────────────────────────────────────────────────────────┘

object TheProblem:
  // Without neotype, these are all just Long
  def getUser(userId: Long): String   = s"User $userId"
  def getOrder(orderId: Long): String = s"Order $orderId"

  val userId: Long  = 123L
  val orderId: Long = 456L

  // Oops! Wrong ID, but compiles fine. Bug waiting to happen.
  getUser(orderId) // No compile error - both are Long!

// ┌─────────────────────────────────────────────────────────────────────────────┐
// │ PART 2: THE SOLUTION (NEWTYPE)                                              │
// │                                                                             │
// │ Newtypes wrap primitives in distinct types that exist only at compile time. │
// │ At runtime, a UserId IS a Long. No boxing, no wrapper objects, no overhead. │
// │ The JVM sees plain primitives. You get type safety completely for free.     │
// └─────────────────────────────────────────────────────────────────────────────┘

type UserId = UserId.Type
object UserId extends Newtype[Long]

type OrderId = OrderId.Type
object OrderId extends Newtype[Long]

object TheSolution:
  def getUser(userId: UserId): String    = s"User $userId"
  def getOrder(orderId: OrderId): String = s"Order $orderId"

  val userId  = UserId(123L)
  val orderId = OrderId(456L)

  getUser(userId)   // ✓ Compiles
  getOrder(orderId) // ✓ Compiles
  // getUser(orderId) // ✗ Won't compile! Type mismatch.

  // Zero-cost: at runtime userId is just a Long, no wrapper object
  val raw: Long = userId.unwrap

// ┌─────────────────────────────────────────────────────────────────────────────┐
// │ PART 3: VALIDATION (CATCH BUGS AT COMPILE TIME)                             │
// │                                                                             │
// │ Add validation rules. Invalid literals fail to compile.                     │
// └─────────────────────────────────────────────────────────────────────────────┘

type Port = Port.Type
object Port extends Newtype[Int]:
  override inline def validate(value: Int) =
    if value >= 1 && value <= 65535 then true
    else s"Port must be 1-65535, got: $value"

// Validation logic is evaluated at compile time (see SUPPORTED.md for limits)
type Email = Email.Type
object Email extends Newtype[String]:
  override inline def validate(value: String) =
    val atIndex  = value.indexOf('@')
    val dotIndex = value.lastIndexOf('.')
    if atIndex < 1 then "Email must have characters before @"
    else if dotIndex < atIndex + 2 then "Email must have domain after @"
    else if dotIndex >= value.length - 2 then "Email must have TLD after ."
    else true

object CompileTimeValidation:
  // ─── Valid literals compile successfully ───
  val http   = Port(80)
  val https  = Port(443)
  val dev    = Port(3000)
  val custom = Port(8080)

  val alice = Email("alice@example.com")
  val bob   = Email("bob@company.org")

  // ─── Invalid literals fail at compile time! ───
  // Uncomment any line below to see the compile error:

  // val bad1 = Port(0) // error: Port must be 1-65535, got: 0
  // val bad2 = Port(70000)   // error: Port must be 1-65535, got: 70000
  // val bad3 = Port(-1)      // error: Port must be 1-65535, got: -1

  // val bad4 = Email("noatsign") // error: Email must have characters before @
  // val bad5 = Email("@nodomain.com")   // error: Email must have characters before @
  // val bad6 = Email("no@tld")          // error: Email must have TLD after .

  // The validation logic runs at compile time. Bugs are caught before your code runs!

// ┌─────────────────────────────────────────────────────────────────────────────┐
// │ PART 4: RUNTIME VALUES (USER INPUT, API RESPONSES, ETC.)                    │
// │                                                                             │
// │ Literals are validated at compile time. But what about runtime values?      │
// │ Use .make for safe runtime construction.                                    │
// └─────────────────────────────────────────────────────────────────────────────┘

object RuntimeValidation:
  // For values not known at compile time (user input, config files, API responses),
  // use .make, .makeOrThrow, or .unsafeMake instead of apply.
  def getPortFromConfig(): Int = 8080 // Imagine this reads from a file
  def getBadPort(): Int        = 99999

  val userInput: Int = getPortFromConfig()
  val badInput: Int  = getBadPort()

  // .make returns Either[String, Port]
  val good: Either[String, Port] = Port.make(userInput) // Right(8080)
  val bad: Either[String, Port]  = Port.make(badInput)  // Left("Port must be 1-65535, got: 99999")

  // .makeOrThrow for tests/scripts (throws IllegalArgumentException on failure)
  val testPort: Port = Port.makeOrThrow(3000)

  // .unsafeMake bypasses validation (only use for already-validated data)
  val trustedPort: Port = Port.unsafeMake(8080)

// ┌─────────────────────────────────────────────────────────────────────────────┐
// │ PART 5: SUBTYPE VS NEWTYPE                                                  │
// │                                                                             │
// │ Newtype: Completely separate type. Need .unwrap to use as underlying.       │
// │ Subtype: IS-A relationship. Can use directly as the underlying type.        │
// └─────────────────────────────────────────────────────────────────────────────┘

// Subtype: PositiveInt <: Int
type PositiveInt = PositiveInt.Type
object PositiveInt extends Subtype[Int]:
  override inline def validate(value: Int) =
    if value > 0 then true else s"Must be positive, got: $value"

object SubtypeVsNewtype:
  val port: Port          = Port(8080)
  val posInt: PositiveInt = PositiveInt(42)

  // Newtype: Port is NOT an Int
  // val x: Int = port        // ✗ Won't compile
  val x: Int = port.unwrap // ✓ Need .unwrap

  // Subtype: PositiveInt IS an Int
  val y: Int = posInt     // ✓ Works directly!
  val z: Int = posInt * 2 // ✓ Can use Int operations (result is Int, not PositiveInt)

  def needsInt(n: Int): Int = n + 1
  // needsInt(port)           // ✗ Port is not an Int
  needsInt(posInt) // ✓ PositiveInt is an Int

// ═══════════════════════════════════════════════════════════════════════════════
// MORE EXAMPLES
// ═══════════════════════════════════════════════════════════════════════════════
//
// The examples below show common validation patterns. They use apply() with
// literals for compile-time validation. For runtime values, use .make instead.

// ─── Validated Strings ───

type NonEmptyString = NonEmptyString.Type
object NonEmptyString extends Newtype[String]:
  override inline def validate(value: String) =
    if value.nonEmpty then true else "String cannot be empty"

type Username = Username.Type
object Username extends Newtype[String]:
  override inline def validate(value: String) =
    if value.length < 3 then "Username must be at least 3 characters"
    else if value.length > 20 then "Username must be at most 20 characters"
    else if !value.forall(c => c.isLetterOrDigit || c == '_') then "Username must be alphanumeric (underscores allowed)"
    else true

type HexColor = HexColor.Type
object HexColor extends Newtype[String]:
  override inline def validate(value: String) =
    val isHex = value.drop(1).forall(c => c.isDigit || "abcdefABCDEF".contains(c))
    if !value.startsWith("#") then "Hex color must start with #"
    else if value.length != 4 && value.length != 7 then "Hex color must be #RGB or #RRGGBB"
    else if !isHex then "Hex color must contain only hex digits"
    else true

type Slug = Slug.Type
object Slug extends Newtype[String]:
  override inline def validate(value: String) =
    if value.isEmpty then "Slug cannot be empty"
    else if !value.forall(c => c.isLower || c.isDigit || c == '-') then
      "Slug must be lowercase letters, digits, and hyphens only"
    else true

object StringExamples:
  val title = NonEmptyString("Hello")
  val user  = Username("kit_langton")
  val color = HexColor("#ff5733")
  val slug  = Slug("my-awesome-post")

// ─── Validated Numbers ───

type Percentage = Percentage.Type
object Percentage extends Newtype[Double]:
  override inline def validate(value: Double) =
    if value >= 0 && value <= 100 then true
    else s"Percentage must be 0-100, got: $value"

type Latitude = Latitude.Type
object Latitude extends Newtype[Double]:
  override inline def validate(value: Double) =
    if value >= -90 && value <= 90 then true
    else s"Latitude must be -90 to 90, got: $value"

type Longitude = Longitude.Type
object Longitude extends Newtype[Double]:
  override inline def validate(value: Double) =
    if value >= -180 && value <= 180 then true
    else s"Longitude must be -180 to 180, got: $value"

object NumberExamples:
  val discount = Percentage(15.5)
  case class Coordinate(lat: Latitude, long: Longitude)
  val sf = Coordinate(Latitude(37.7749), Longitude(-122.4194))

// ─── Validated Collections ───

type NonEmptyList = NonEmptyList.Type
object NonEmptyList extends Newtype[List[String]]:
  override inline def validate(value: List[String]) =
    if value.nonEmpty then true else "List cannot be empty"

type PositiveIntList = PositiveIntList.Type
object PositiveIntList extends Newtype[List[Int]]:
  override inline def validate(value: List[Int]) =
    if value.forall(_ > 0) then true else "All elements must be positive"

object CollectionExamples:
  val names  = NonEmptyList(List("Alice", "Bob"))
  val counts = PositiveIntList(List(1, 2, 3))
