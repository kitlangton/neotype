package neotype

import zio.test.*

import scala.compiletime.summonInline
import scala.compiletime.testing.*
import scala.quoted.*

type NameWrapper = NameWrapper.Type
given NameWrapper: Newtype.Simple[String] with {}

object NewtypeSimpleSpec extends ZIOSpecDefault:
  val spec = suite("NewtypeSpec")(
    test("simple") {
      val nameWrapper = NameWrapper("Bobo The Clown")
      assertTrue(
        nameWrapper.unwrap == "Bobo The Clown"
      )
    }
  )
