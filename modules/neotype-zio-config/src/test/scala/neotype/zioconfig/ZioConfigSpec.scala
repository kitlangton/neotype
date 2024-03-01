package neotype.zioconfig

import neotype.*
import zio.test.*
import zio.config.*
import zio.config.magnolia.*
import zio.ConfigProvider
import neotype.test.definitions.*

object ZioConfigSpec extends ZIOSpecDefault:
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
    }
  )
