# neotype

[![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
[![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots]

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/io.github.kitlangton/neotype_3.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/io.github.kitlangton/neotype_3.svg "Sonatype Snapshots"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/io/github/kitlangton/neotype_3/ "Sonatype Snapshots"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/io/github/kitlangton/neotype_3/ "Sonatype Releases"

A friendly newtype library for Scala 3.

```scala
"io.github.kitlangton" %% "neotype" % "0.0.6"
```

## Features

- Compile-time Checked Values
- Write validations as **plain, old Scala expressions**
- Very Helpful Error Messages (_see below_)
- No Runtime Allocations (Thanks to `inline` and `opaque type`)
- Ability to integrate with other libraries (e.g. `zio-json`, `circe`, etc.)

### Example

```scala
import neotype.*

// Define a newtype:
given NonEmptyString: Newtype[String] with
  // Hey, a plain old Scala expression!
  inline def validate(input: String): Boolean =
    input.nonEmpty

// Wrap values, which checked at compile-time:
NonEmptyString("Hello") // OK
NonEmptyString("")      // Compile Error
```

```scala
Error: /Users/kit/code/neotype/examples/src/main/scala/neotype/examples/Main.scala:9:16 
  NonEmptyString("")                  
  ^^^^^^^^^^^^^^^^^^
  —— Newtype Error ——————————————————————————————————————————————————————————
  NonEmptyString was called with an INVALID String.
  input: ""
  check: input.nonEmpty
  ———————————————————————————————————————————————————————————————————————————
```

## Integrations

Neotype integrates with the following libraries.

- zio-json
- zio-config
- tapir
- quill
- circe

### ZIO Json Example

```scala
import neotype.*
import neotype.ziojson.*
import zio.json.*

type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  inline def validate(value: String): Boolean = value.nonEmpty
  override inline def failureMessage = "String must not be empty"

case class Person(name: NonEmptyString, age: Int) derives JsonCodec

val parsed = """{"name": "Kit", "age": 30}""".fromJson[Person]
// Right(Person(NonEmptyString("Kit"), 30))

val failed = """{"name": "", "age": 30}""".fromJson[Person]
// Left(".name(String must not be empty)")
```

By importing `neotype.ziojson.*`, we automatically generate a `JsonCodec` for `NonEmptyString`. Custom
failure messages are also supported (by overriding `def failureMessage` in the Newtype definition).