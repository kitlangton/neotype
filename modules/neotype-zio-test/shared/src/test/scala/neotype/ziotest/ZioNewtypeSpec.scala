package neotype.ziotest

import neotype.{Newtype, Subtype}
import zio.test.*
import zio.*
import zio.test.magnolia.DeriveGen

type SimpleNewtype = SimpleNewtype.Type
given SimpleNewtype: Newtype.Simple[Int]()

type SimpleSubtype = SimpleSubtype.Type
given SimpleSubtype: Subtype.Simple[String]()

object ZioSpec extends ZIOSpecDefault:

  final case class Composed(
      name: SimpleNewtype,
      nickname: SimpleSubtype
  )

  val composedGen: Gen[Any, Composed] = DeriveGen[Composed]

  def spec =
    suite("ZioTestSpec")(
      test("Derives Gen") {
        check(composedGen) { case Composed(name, nickname) =>
          assertTrue(name == name, nickname == nickname)
        }
      }
    )
