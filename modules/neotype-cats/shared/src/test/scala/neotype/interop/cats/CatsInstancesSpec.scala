package neotype.interop.cats

import cats.Eq
import cats.Show
import cats.implicits.*
import neotype.Newtype
import neotype.Subtype
import neotype.common.NonEmptyString
import neotype.test.definitions.*
import zio.test.*

object CatsInstancesSpec extends ZIOSpecDefault:

  // Test for stacked newtypes
  type FullName = FullName.Type
  object FullName extends Newtype[NonEmptyString]

  def spec = suite("CatsInstancesSpec")(
    suite("Show instances")(
      test("Show for simple newtype") {
        val simpleNewtype = SimpleNewtype(42)
        assertTrue(Show[SimpleNewtype].show(simpleNewtype) == "42")
      },
      test("Show for subtype") {
        val simpleSubtype = SimpleSubtype(10)
        assertTrue(Show[SimpleSubtype].show(simpleSubtype) == "10")
      },
      test("Show for stacked newtype") {
        val fullName = FullName(NonEmptyString("Bob Smith"))
        assertTrue(Show[FullName].show(fullName) == "Bob Smith")
      }
    ),
    suite("Eq instances")(
      test("Eq for simple newtype") {
        val simple1 = SimpleNewtype(42)
        val simple2 = SimpleNewtype(42)
        val simple3 = SimpleNewtype(43)
        assertTrue(
          Eq[SimpleNewtype].eqv(simple1, simple2),
          Eq[SimpleNewtype].neqv(simple1, simple3)
        )
      },
      test("Eq for subtype") {
        val subtype1 = SimpleSubtype(10)
        val subtype2 = SimpleSubtype(10)
        val subtype3 = SimpleSubtype(20)
        assertTrue(
          Eq[SimpleSubtype].eqv(subtype1, subtype2),
          Eq[SimpleSubtype].neqv(subtype1, subtype3)
        )
      },
      test("Eq for stacked newtype") {
        val fullName1 = FullName(NonEmptyString("Bob Smith"))
        val fullName2 = FullName(NonEmptyString("Bob Smith"))
        val fullName3 = FullName(NonEmptyString("Alice Jones"))
        assertTrue(
          Eq[FullName].eqv(fullName1, fullName2),
          Eq[FullName].neqv(fullName1, fullName3)
        )
      }
    )
  )
