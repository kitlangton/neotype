# neotype

[![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
[![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots]

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/io.github.kitlangton/neotype_3.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/io.github.kitlangton/neotype_3.svg "Sonatype Snapshots"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/io/github/kitlangton/neotype_3/ "Sonatype Snapshots"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/io/github/kitlangton/neotype_3/ "Sonatype Releases"

A friendly newtype library for Scala 3.

```scala
"io.github.kitlangton" %% "neotype" % "0.3.21"
```

## Features

- Compile-time Checked Values
- Write validations as **plain, old Scala expressions**
- Helpful compilation errors (_see below_)
- No runtime allocations (Thanks to `inline` and `opaque type`)
- Integrates with other libraries (e.g. `zio-json`, `circe`, `tapir`, etc.)

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
  —— Newtype Error ——————————————————————————————————————————————————————————
  NonEmptyString was called with an INVALID String.
  input: ""
  check: input.nonEmpty
  ———————————————————————————————————————————————————————————————————————————
```

## Integrations

Neotype integrates with the following libraries:

- **JSON**
  - [zio-json](https://github.com/zio/zio-json)
  - [play-json](https://github.com/playframework/play-json)
  - [jsoniter](https://github.com/plokhotnyuk/jsoniter-scala)
  - [circe](https://github.com/circe/circe)
  - [upickle](https://github.com/lihaoyi/upickle)
- **DATABASE**
  - [doobie](https://github.com/tpolecat/doobie)
  - [quill](https://github.com/zio/zio-quill)
- **MISCELLANEOUS**
  - [zio-test](https://github.com/zio/zio) `DeriveGen`
  - [zio-config](https://github.com/zio/zio-config)
  - [zio-schema](https://github.com/zio/zio-schema)
  - [tapir](https://github.com/softwaremill/tapir)
  - [chimney](https://github.com/scalalandio/chimney)
  - [caliban](https://github.com/ghostdogpr/caliban)
  - [ciris](https://github.com/vlovgr/ciris)

### ZIO Json Example

```scala
import neotype.*

type NonEmptyString = NonEmptyString.Type
object NonEmptyString extends Newtype[String]:
  override inline def validate(value: String): Result =
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

By importing `neotype.ziojson.given`, we automatically generate a `JsonCodec` for `NonEmptyString`. Custom
failure messages are also supported (by overriding `def failureMessage` in the Newtype definition).
Note that `import neotype.interop.ziojson.given` needs to be in the same file as `Person`, _not_ `NonEmptyString`.
The generated `JsonCodec` is not made available to the entire project, but only to the file where it is imported.
