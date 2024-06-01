package neotype.interop.ziotest

import neotype.test.definitions.*
import zio.*
import zio.test.*
import zio.test.magnolia.DeriveGen

object ZioSpec extends ZIOSpecDefault:

  final case class Composed(name: SimpleNewtype, nickname: SimpleSubtype)

  val composedGen: Gen[Any, Composed] = DeriveGen[Composed]

  def spec =
    suite("ZioTestSpec")(
      test("Derives Gen") {
        check(composedGen) { case Composed(name, nickname) =>
          assertTrue(name == name, nickname == nickname)
        }
      }
    )
