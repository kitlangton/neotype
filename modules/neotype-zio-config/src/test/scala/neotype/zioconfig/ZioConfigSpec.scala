package neotype.zioconfig

import neotype.{Newtype, Subtype}
import zio.test.*
import zio.config.*
import zio.config.magnolia.*

type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  inline def validate(value: String): Boolean =
    value.nonEmpty

  override inline def failureMessage = "String must not be empty"

type SubtypeLongString = SubtypeLongString.Type
given SubtypeLongString: Subtype[String] with
  inline def validate(value: String): Boolean =
    value.length > 10

  override inline def failureMessage = "String must be longer than 10 characters"

type SimpleNewtype = SimpleNewtype.Type
given SimpleNewtype: Newtype.Simple[Int]()

type SimpleSubtype = SimpleSubtype.Type
given SimpleSubtype: Subtype.Simple[String]()

final case class MyConfig(
    nonEmptyString: NonEmptyString,
    subtypeLongString: SubtypeLongString,
    simpleNewtype: SimpleNewtype,
    simpleSubtype: SimpleSubtype
)

object ZioJsonSpec extends ZIOSpecDefault:
  def spec = suite("ZioConfigSpec")(
    test("successfully read config") {
      val expectedConfig = MyConfig(
        NonEmptyString("hello"),
        SubtypeLongString("hello world"),
        SimpleNewtype(1),
        SimpleSubtype("hello world")
      )

      val source = ConfigSource.fromMap(
        Map(
          "nonEmptyString"    -> "hello",
          "subtypeLongString" -> "hello world",
          "simpleNewtype"     -> "1",
          "simpleSubtype"     -> "hello world"
        )
      )

      for actualConfig <- read(descriptor[MyConfig] from source)
      yield assertTrue(actualConfig == expectedConfig)
    },
    test("fails to read config") {
      val source = ConfigSource.fromMap(
        Map(
          "nonEmptyString"    -> "",
          "subtypeLongString" -> "hello",
          "simpleNewtype"     -> "1",
          "simpleSubtype"     -> "hello world"
        )
      )

      for actualConfig <- read(descriptor[MyConfig] from source).either
      yield assertTrue(
        actualConfig.is(_.left).getMessage.contains("String must not be empty"),
        actualConfig.is(_.left).getMessage.contains("String must be longer than 10 characters")
      )
    }
  )
