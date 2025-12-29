# neotype

[![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
[![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots]

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/io.github.kitlangton/neotype_3.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/io.github.kitlangton/neotype_3.svg "Sonatype Snapshots"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/io/github/kitlangton/neotype_3/ "Sonatype Snapshots"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/io/github/kitlangton/neotype_3/ "Sonatype Releases"

A friendly newtype library for Scala 3.

```scala
"io.github.kitlangton" %% "neotype" % "x.y.z"
"io.github.kitlangton" %% "comptime" % "x.y.z" // optional
```

Use the latest version shown in the Sonatype badge above.

## Features

- Compile-time checked values using **plain Scala expressions**
- Helpful compilation errors (_see below_)
- Zero runtime overhead (thanks to `inline` and `opaque type`)
- Runtime validation via `make` and `makeOrThrow`
- Integrations with popular libraries (e.g. `zio-json`, `circe`, `tapir`)
- Optional `comptime` engine for compile-time evaluation (see below)

## [5 Minute Video Tour](http://www.youtube.com/watch?v=6AxSX_WX7ek)

<a href="http://www.youtube.com/watch?v=6AxSX_WX7ek" title="NEOTYPE TOUR">
    <img src="https://img.youtube.com/vi/6AxSX_WX7ek/maxresdefault.jpg" alt="NEOTYPE VIDEO TOUR" width="500" height="300">
</a>

## Example

Here is how to define a compile-time validated Newtype.

```scala
import neotype.*

// 1. Define a newtype.
object NonEmptyString extends Newtype[String]:

  // 2. Optionally, define a validate method.
  override inline def validate(input: String): Boolean =
    input.nonEmpty

// 3. Construct values.
NonEmptyString("Hello") // OK
NonEmptyString("")      // Compile Error
```

Attempting to call `NonEmptyString("")` would result in the following compilation error:

```scala
Error: /src/main/scala/examples/Main.scala:9:16
  NonEmptyString("")
  ^^^^^^^^^^^^^^^^^^
  —— Neotype Error ——————————————————————————————————————————————————————————
  NonEmptyString was called with an INVALID String.
  input: ""
  check: input.nonEmpty
  ———————————————————————————————————————————————————————————————————————————
```

### Real-World Examples

```scala
import neotype.*

// Type-safe IDs - prevent mixing up different entity types
type UserId = UserId.Type
object UserId extends Newtype[Long]

type OrderId = OrderId.Type
object OrderId extends Newtype[Long]

def getUser(id: UserId): User = ...
def getOrder(id: OrderId): Order = ...

getUser(UserId(123))   // ✓ Compiles
getUser(OrderId(456))  // ✗ Won't compile - type mismatch!

// Bounded numbers with validation
type Port = Port.Type
object Port extends Newtype[Int]:
  override inline def validate(value: Int) =
    if value >= 1 && value <= 65535 then true
    else s"Port must be 1-65535, got: $value"

Port(8080)   // ✓ Compiles
Port(99999)  // ✗ Compile error: Port must be 1-65535

// Validated strings
type Username = Username.Type
object Username extends Newtype[String]:
  override inline def validate(value: String) =
    if value.length < 3 then "Username must be at least 3 characters"
    else if !value.forall(_.isLetterOrDigit) then "Username must be alphanumeric"
    else true

// Geographic coordinates
type Latitude = Latitude.Type
object Latitude extends Newtype[Double]:
  override inline def validate(value: Double) =
    if value >= -90 && value <= 90 then true
    else s"Latitude must be -90 to 90, got: $value"
```

See [`examples/src/main/scala/examples/NewtypeExamples.scala`](examples/src/main/scala/examples/NewtypeExamples.scala) for more examples including validated strings, subtypes, runtime validation, and collections.

## Integrations

Neotype integrates with the following libraries:

- **JSON**
  - [zio-json](https://github.com/zio/zio-json)
  - [play-json](https://github.com/playframework/play-json)
  - [jsoniter](https://github.com/plokhotnyuk/jsoniter-scala)
  - [circe](https://github.com/circe/circe)
  - [upickle](https://github.com/lihaoyi/upickle)
  - [tethys](https://github.com/tethys-json/tethys)
- **DATABASE / STORAGE**
  - [doobie](https://github.com/tpolecat/doobie)
  - [zio-quill](https://github.com/zio/zio-quill)
  - [scanamo](https://github.com/scanamo/scanamo)
- **CONFIG**
  - [zio-config](https://github.com/zio/zio-config)
  - [pureconfig](https://github.com/pureconfig/pureconfig)
  - [ciris](https://github.com/vlovgr/ciris)
- **ZIO**
  - [zio-test](https://github.com/zio/zio) `DeriveGen`
  - [zio-schema](https://github.com/zio/zio-schema)
- **MISCELLANEOUS**
  - [tapir](https://github.com/softwaremill/tapir)
  - [chimney](https://github.com/scalalandio/chimney)
  - [caliban](https://github.com/ghostdogpr/caliban)
  - [cats](https://github.com/typelevel/cats) `Show`, `Eq`, `Order`

### ZIO Json Example

```scala
import neotype.*

type NonEmptyString = NonEmptyString.Type
object NonEmptyString extends Newtype[String]:
  override inline def validate(value: String): Boolean | String =
    if value.nonEmpty then true else "String must not be empty"
```

```scala
import neotype.interop.ziojson.given
import zio.json.*

case class Person(name: NonEmptyString, age: Int) derives JsonCodec

val parsed = """{"name": "Kit", "age": 30}""".fromJson[Person]
// Right(Person(NonEmptyString("Kit"), 30))

val failed = """{"name": "", "age": 30}""".fromJson[Person]
// Left(".name(String must not be empty)")
```

By importing `neotype.interop.ziojson.given`, we automatically generate a `JsonCodec` for `NonEmptyString`. Custom
failure messages are also supported (by overriding `def failureMessage` in the Newtype definition).
Note that `import neotype.interop.ziojson.given` needs to be in the same file as `Person`, _not_ `NonEmptyString`.
The generated `JsonCodec` is not made available to the entire project, but only to the file where it is imported.

## Comptime

Neotype ships an optional `comptime` module, a compile-time evaluator inspired by Zig's `comptime`.
It evaluates expressions at compile time and inlines the results as literals.

```scala
import comptime.*

val primes = comptime {
  (2 to 50).toList.filter(n => (2 until n).forall(n % _ != 0))
}
// Compiles to: List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)
```

### Compile-Time Parsing with Code Reuse

Parse and validate data at compile time. The `inline def` pattern lets you share logic
between compile-time and runtime versions:

```scala
import comptime.*

final case class SemVer(major: Int, minor: Int, patch: Int)

object SemVer:
  // Shared parsing logic - inline so it works in both contexts
  private inline def doParse(s: String): Either[String, SemVer] =
    val parts = s.split("\\.").toList
    parts match
      case List(maj, min, pat) => Right(SemVer(maj.toInt, min.toInt, pat.toInt))
      case _ => Left(s"Invalid semver: $s")

  // COMPILE-TIME: invalid input = compile error
  inline def parse(inline s: String): SemVer = comptime {
    doParse(s).fold(comptimeError(_), identity)
  }

  // RUNTIME: for user input, returns Either
  def parseEither(s: String): Either[String, SemVer] = doParse(s)

// Compile-time - literal in bytecode
val version = SemVer.parse("1.2.3")  // → SemVer(1, 2, 3)

// Runtime - graceful error handling
SemVer.parseEither(userInput) match
  case Right(v) => println(s"Valid: $v")
  case Left(e)  => println(s"Error: $e")
```

### Static Assertions

Validate relationships between configuration constants at compile time.
Use `inline val` to define constants, then assert their invariants:

```scala
import comptime.*

inline def staticAssert(inline cond: Boolean, inline msg: String): Unit =
  comptime { if !cond then comptimeError(msg) }

object Config:
  inline val BUFFER_SIZE = 4096
  inline val MAX_ITEMS   = 100
  inline val ITEM_SIZE   = 40

  // Catches bugs when ANY constant changes!
  staticAssert(
    BUFFER_SIZE >= MAX_ITEMS * ITEM_SIZE,
    "Buffer too small for max items"
  )

  staticAssert(
    (BUFFER_SIZE & (BUFFER_SIZE - 1)) == 0,
    "Buffer size must be power of 2"
  )
```

### Custom Compile Errors

Use `comptimeError` to produce descriptive compile errors:

```scala
inline def parsePort(inline s: String): Int = comptime {
  val port = s.toInt
  if port < 1 || port > 65535 then
    comptimeError(s"Invalid port: $port. Must be 1-65535")
  port
}

parsePort("8080")   // ✓ Compiles to: 8080
parsePort("99999")  // ✗ Compile error: Invalid port: 99999
```

### More Examples

See [`examples/src/main/scala/examples/ComptimeExamples.scala`](examples/src/main/scala/examples/ComptimeExamples.scala) for:
- Pre-computed lookup tables (primes, factorials)
- Duration literals (`"30s"` → `Duration.ofSeconds(30)`)
- Regex validation at compile time
- Protocol buffer sizing assertions
- Feature flag dependency checking

### Supported Operations

- **Primitives**: arithmetic, comparisons, boolean logic, bitwise ops
- **Strings**: `split`, `trim`, `toInt`, `substring`, `contains`, `matches`, etc.
- **Collections**: `List`, `Vector`, `Set`, `Map` with `map`, `filter`, `fold`, etc.
- **Control flow**: `if`/`else`, pattern matching, `try`/`catch`, `val` bindings
- **Case classes**: construction, field access, pattern matching
- **Options/Eithers/Try**: `Some`, `None`, `Right`, `Left`, `Success`, `Failure`, etc.
- **java.time**: `Duration`, `LocalDate`, `LocalTime` construction and operations
- **Regex**: `findFirstIn`, `findAllIn`, `replaceAllIn`, `matches`, etc.

See [SUPPORTED.md](modules/comptime/SUPPORTED.md) for the complete list.
