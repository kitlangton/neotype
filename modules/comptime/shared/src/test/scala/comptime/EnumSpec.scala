package comptime

import _root_.comptime.fixtures.*
import zio.test.*

object EnumSpec extends ZIOSpecDefault:
  val spec =
    suite("EnumSpec")(
      suite("basic enum support")(
        test("simple enum in case class") {
          val pos = GridPos.parse("top-left")
          assertTrue(pos == GridPos(Row.Top, Col.Left))
        },
        test("all enum combinations") {
          assertTrue(
            GridPos.parse("top-left") == GridPos(Row.Top, Col.Left),
            GridPos.parse("middle-center") == GridPos(Row.Middle, Col.Center),
            GridPos.parse("bottom-right") == GridPos(Row.Bottom, Col.Right)
          )
        }
      ),
      suite("enum with overridden toString")(
        test("works despite custom toString") {
          // ColorWithToString.Red.toString == "COLOR_RED"
          // but productPrefix == "Red" which is what we use
          val box = ColorBox.make("red")
          assertTrue(box.color == ColorWithToString.Red)
        },
        test("all colors work") {
          assertTrue(
            ColorBox.make("red").color == ColorWithToString.Red,
            ColorBox.make("green").color == ColorWithToString.Green,
            ColorBox.make("blue").color == ColorWithToString.Blue
          )
        }
      ),
      suite("parameterized enum cases")(
        test("enum with constructor params") {
          // HttpStatus.Ok extends HttpStatus(200)
          val box = StatusBox.make("ok")
          assertTrue(box.status == HttpStatus.Ok, box.status.code == 200)
        },
        test("all parameterized cases work") {
          assertTrue(
            StatusBox.make("ok").status == HttpStatus.Ok,
            StatusBox.make("notfound").status == HttpStatus.NotFound,
            StatusBox.make("error").status == HttpStatus.ServerError
          )
        },
        test("enum values have correct params") {
          assertTrue(
            StatusBox.make("ok").status.code == 200,
            StatusBox.make("notfound").status.code == 404,
            StatusBox.make("error").status.code == 500
          )
        }
      )
    )
