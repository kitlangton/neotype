package neotype.zioconfig

import neotype.*
import zio.test.*
import zio.config.*
import zio.config.magnolia.*
import zio.ConfigProvider

type Name = Name.Type
object Name extends Newtype[String]

type NonEmptyString = NonEmptyString.Type
object NonEmptyString extends Newtype[String]:

  override inline def validate(value: String): Boolean =
    value.nonEmpty

  override inline def failureMessage = "String must not be empty"

type SubtypeLongString = SubtypeLongString.Type
object SubtypeLongString extends Subtype[String]:

  override inline def validate(value: String): Boolean =
    value.length > 10

  override inline def failureMessage = "String must be longer than 10 characters"

type SimpleNewtype = SimpleNewtype.Type
object SimpleNewtype extends Newtype.Simple[Int]

type SimpleSubtype = SimpleSubtype.Type
object SimpleSubtype extends Subtype.Simple[String]

final case class MyConfig(
    nonEmptyString: NonEmptyString,
    subtypeLongString: SubtypeLongString,
    simpleNewtype: SimpleNewtype,
    simpleSubtype: SimpleSubtype
)

object ZioConfigSpec extends ZIOSpecDefault:
  def spec = suite("zio-config")(
    test("successfully read config") {
      val expectedConfig = MyConfig(
        NonEmptyString("hello"),
        SubtypeLongString("hello world"),
        SimpleNewtype(1),
        SimpleSubtype("hello world")
      )

      val source = ConfigProvider.fromMap(
        Map(
          "nonEmptyString"    -> "hello",
          "subtypeLongString" -> "hello world",
          "simpleNewtype"     -> "1",
          "simpleSubtype"     -> "hello world"
        )
      )

      for actualConfig <- read(deriveConfig[MyConfig] from source)
      yield assertTrue(actualConfig == expectedConfig)
    },
    test("fails to read config") {
      val source = ConfigProvider.fromMap(
        Map(
          "nonEmptyString"    -> "",
          "subtypeLongString" -> "short",
          "simpleNewtype"     -> "1",
          "simpleSubtype"     -> "hello world"
        )
      )

      for actualConfig <- read(deriveConfig[MyConfig] from source).either
      yield assertTrue(
        actualConfig
          .is(_.left)
          .getMessage
          .contains("String must not be empty"),
        actualConfig.is(_.left).getMessage.contains("String must be longer than 10 characters")
      )
    }
  )
