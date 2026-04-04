package neotype.interop.zioconfig

import neotype.*
import neotype.test.definitions.*
import zio.ConfigProvider
import zio.config.*
import zio.config.magnolia.*
import zio.test.*

object ZioConfigSpec extends ZIOSpecDefault:
  case class ValidatedMapConfig(entries: Map[ValidatedNewtype, Int])
  case class ValidatedSubtypeMapConfig(entries: Map[ValidatedSubtype, Int])
  case class NewtypeValidatedSubtypeMapConfig(entries: Map[NewtypeValidatedSubtype, Int])
  case class TwiceValidatedSubtypeMapConfig(entries: Map[TwiceValidatedSubtype, Int])

  type SimpleStringNewtype = SimpleStringNewtype.Type
  object SimpleStringNewtype extends Newtype[String]

  case class SimpleMapConfig(entries: Map[SimpleStringNewtype, Int])

  def spec = suite("zio-config")(
    test("successfully read config") {
      val expectedConfig = Composite(
        ValidatedNewtype("hello"),
        SimpleNewtype(1),
        ValidatedSubtype("hello world"),
        SimpleSubtype(123456)
      )

      val source = ConfigProvider.fromMap(
        Map(
          "newtype"       -> "hello",
          "simpleNewtype" -> "1",
          "subtype"       -> "hello world",
          "simpleSubtype" -> "123456"
        )
      )

      for config <- read(deriveConfig[Composite] from source)
      yield assertTrue(config == expectedConfig)
    },
    test("fails to read config") {
      val source = ConfigProvider.fromMap(
        Map(
          "newtype"       -> "",
          "simpleNewtype" -> "1",
          "subtype"       -> "hello",
          "simpleSubtype" -> "123456"
        )
      )

      for config <- read(deriveConfig[Composite] from source).either
      yield assertTrue(
        config.is(_.left).getMessage.contains("String must not be empty"),
        config.is(_.left).getMessage.contains("String must be longer than 10 characters")
      )
    },
    suite("Map")(
      test("validated map keys") {
        val expectedConfig = ValidatedMapConfig(
          Map(ValidatedNewtype("hello") -> 1, ValidatedNewtype("world") -> 2)
        )

        val source = ConfigProvider.fromMap(
          Map(
            "entries.hello" -> "1",
            "entries.world" -> "2"
          )
        )

        for config <- read(deriveConfig[ValidatedMapConfig] from source)
        yield assertTrue(config == expectedConfig)
      },
      test("simple map keys") {
        val expectedConfig = SimpleMapConfig(
          Map(SimpleStringNewtype("foo") -> 1, SimpleStringNewtype("bar") -> 2)
        )

        val source = ConfigProvider.fromMap(
          Map(
            "entries.foo" -> "1",
            "entries.bar" -> "2"
          )
        )

        for config <- read(deriveConfig[SimpleMapConfig] from source)
        yield assertTrue(config == expectedConfig)
      },
      test("fails with invalid validated map keys") {
        val source = ConfigProvider.fromMap(
          Map(
            "entries.short" -> "1"
          )
        )

        for config <- read(deriveConfig[ValidatedSubtypeMapConfig] from source).either
        yield assertTrue(
          config.is(_.left).getMessage.contains("String must be longer than 10 characters")
        )
      },
      test("NewtypeValidatedSubtype map keys") {
        val expectedConfig = NewtypeValidatedSubtypeMapConfig(
          Map(NewtypeValidatedSubtype(ValidatedSubtype("long enough key")) -> 1)
        )

        val source = ConfigProvider.fromMap(
          Map("entries.long enough key" -> "1")
        )

        for config <- read(deriveConfig[NewtypeValidatedSubtypeMapConfig] from source)
        yield assertTrue(config == expectedConfig)
      },
      test("TwiceValidatedSubtype map keys") {
        val expectedConfig = TwiceValidatedSubtypeMapConfig(
          Map(TwiceValidatedSubtype.makeOrThrow(ValidatedSubtype("this key is long enough")) -> 1)
        )

        val source = ConfigProvider.fromMap(
          Map("entries.this key is long enough" -> "1")
        )

        for config <- read(deriveConfig[TwiceValidatedSubtypeMapConfig] from source)
        yield assertTrue(config == expectedConfig)
      },
      test("fails with TwiceValidatedSubtype map keys that are too short") {
        val source = ConfigProvider.fromMap(
          Map("entries.twelve chars" -> "1")
        )

        for config <- read(deriveConfig[TwiceValidatedSubtypeMapConfig] from source).either
        yield assertTrue(
          config.is(_.left).getMessage.contains("String must be longer than 15 characters")
        )
      }
    )
  )
