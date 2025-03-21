package neotype.interop.pureconfig

import neotype.Newtype
import neotype.test.definitions.{SimpleNewtype, *}
import pureconfig.{ConfigReader, ConfigSource}
import zio.test.*

object PureconfigSpec extends ZIOSpecDefault:
  case class Config[A](value: A)

  def spec = suite("ChimneySpec")(
    suite("NonEmptyString")(
      test("read success") {
        val config = ConfigSource.string(s"value = \"hello\"")
        val result = config.load[Config[ValidatedNewtype]]
        assertTrue(result == Right(Config(ValidatedNewtype("hello"))))
      },
      test("read failure") {
        val config = ConfigSource.string(s"value = \"\"")
        val result = config.load[Config[ValidatedNewtype]]
        assertTrue(result.isLeft)
      },
    ),
    suite("SubtypeLongString")(
      test("read success") {
        val config = ConfigSource.string(s"value = \"hello world\"")
        val result = config.load[Config[ValidatedSubtype]]
        assertTrue(result == Right(Config(ValidatedSubtype("hello world"))))
      },
      test("read failure") {
        val config = ConfigSource.string(s"value = \"hello\"")
        val result = config.load[Config[ValidatedSubtype]]
        assertTrue(result.isLeft)
      }
    ),
    suite("SimpleNewtype")(
      test("read success") {
        val config = ConfigSource.string(s"value = 123")
        val result = config.load[Config[SimpleNewtype]]
        assertTrue(result == Right(Config(SimpleNewtype(123))))
      },
    ),
    suite("SimpleSubtype")(
      test("read success") {
        val config = ConfigSource.string(s"value = 123")
        val result = config.load[Config[SimpleSubtype]]
        assertTrue(result == Right(Config(SimpleSubtype(123))))
      }
    )
  )

  given [A: ConfigReader]: ConfigReader[Config[A]] =
    ConfigReader.forProduct1[Config[A], A]("value")(Config.apply)
