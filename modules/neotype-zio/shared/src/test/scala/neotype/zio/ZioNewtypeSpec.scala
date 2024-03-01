package neotype.zio

import neotype.{Newtype, Subtype}
import zio.test.*
import zio.*

type MyNewtype = MyNewtype.Type
given MyNewtype: Newtype[String] with
  override inline def validate(value: String): Boolean =
    value.nonEmpty

  override inline def failureMessage = "String must not be empty"

type MySubtype = MySubtype.Type
given MySubtype: Subtype[String] with
  override inline def validate(value: String): Boolean =
    value.length > 10

  override inline def failureMessage = "String must be longer than 10 characters"

type SimpleNewtype = SimpleNewtype.Type
given SimpleNewtype: Newtype.Simple[Int]()

type SimpleSubtype = SimpleSubtype.Type
given SimpleSubtype: Subtype.Simple[String]()

final case class SomeService(
    myNewtype: MyNewtype,
    simpleNewtype: SimpleNewtype
):
  def showAll = ZIO.succeed(s"""
    |myNewtype: $myNewtype
    |simpleNewtype: $simpleNewtype
    |""".stripMargin.trim)

object SomeService:
  val showAll = ZIO.serviceWithZIO[SomeService](_.showAll)
  val layer   = ZLayer.fromFunction(SomeService.apply _)

object ZioSpec extends ZIOSpecDefault:

  def spec =
    val newtypeLayer: ULayer[MyNewtype]           = ZLayer.succeed(MyNewtype("Hello"))
    val simpleNewtypeLayer: ULayer[SimpleNewtype] = ZLayer.succeed(SimpleNewtype(1))
//    val subtypeLayer: ULayer[MySubtype]           = ZLayer.succeed(MySubtype("Hello World"))
//    val combined: ZLayer[Any, Nothing, MyNewtype & SimpleNewtype & MySubtype] =
//      newtypeLayer ++ simpleNewtypeLayer ++ subtypeLayer
    suite("ZioSpec")(
      suite("MyNewtype")(
        test("tag materialization") {
          SomeService.showAll.map { str =>
            assertTrue(
              str ==
                """
                  |myNewtype: Hello
                  |simpleNewtype: 1
                  |""".stripMargin.trim
            )
          }
        }
      ).provide(
        SomeService.layer,
        newtypeLayer ++ simpleNewtypeLayer
        // TODO: Does not work!
//        newtypeLayer,
//        simpleNewtypeLayer
      )
    )
