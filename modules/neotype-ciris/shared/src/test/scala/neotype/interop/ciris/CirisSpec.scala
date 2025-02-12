package neotype.interop.ciris

import cats.syntax.all.*
import ciris.*
import neotype.test.definitions.*
import zio.Task
import zio.interop.catz.*
import zio.test.*

object CirisSpec extends ZIOSpecDefault:
  def spec = suite("ciris")(
    test("successfully read config") {
      val expectedConfig = Composite(
        ValidatedNewtype("hello"),
        SimpleNewtype(1),
        ValidatedSubtype("hello world"),
        SimpleSubtype(123456)
      )

      val source = (
        default("hello").as[ValidatedNewtype],
        default(1).as[SimpleNewtype],
        default("hello world").as[ValidatedSubtype],
        default(123456).as[SimpleSubtype]
      ).parMapN(Composite.apply)

      for config <- source.load[Task]
      yield assertTrue(config == expectedConfig)
    },
    test("fails to read config") {
      val source = (
        default("").as[ValidatedNewtype],
        default(1).as[SimpleNewtype],
        default("hello").as[ValidatedSubtype],
        default(123456).as[SimpleSubtype]
      ).parMapN(Composite.apply)

      for config <- source.load[Task].either
      yield assertTrue(
        config.is(_.left).getMessage.contains("String must not be empty"),
        config.is(_.left).getMessage.contains("String must be longer than 10 characters")
      )
    }
  )
