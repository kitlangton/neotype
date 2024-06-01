package neotype.interop.ziojson

import neotype.*
import zio.json.*
import zio.test.*

type Sandwich = Sandwich.Type
object Sandwich extends Newtype[String]:
  given codec: JsonEncoder[Sandwich] = JsonEncoder.string.contramap[Sandwich](s => s"SANDWICH: $s")

object ZioJsonSpecificSpec extends ZIOSpecDefault:
  def spec = suite("ZioJson Custom")(
    test("custom codec is selected over imported definition") {
      val sandwich: Sandwich = Sandwich("Turkey Chicken Soda Jr.")
      val json               = sandwich.toJson
      assertTrue(json == """"SANDWICH: Turkey Chicken Soda Jr."""")
    }
  )
