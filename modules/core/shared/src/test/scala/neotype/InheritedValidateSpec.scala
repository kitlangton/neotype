package neotype

import zio.test.*

import scala.compiletime.testing.*

/** Test for inherited validate bug. If validate is defined in a parent trait,
  * does compile-time validation still work?
  */
object InheritedValidateSpec extends ZIOSpecDefault:

  // Define validation in a trait that properly overrides TypeWrapper
  trait PositiveIntValidation extends Newtype[Int]:
    override inline def validate(n: Int): Boolean = n > 0

  // Extend the trait - does it pick up validation?
  type InheritedPositive = InheritedPositive.Type
  object InheritedPositive extends PositiveIntValidation

  // Direct validation (control case)
  type DirectPositive = DirectPositive.Type
  object DirectPositive extends Newtype[Int]:
    override inline def validate(n: Int): Boolean = n > 0

  // Edge case: unrelated validate method with different signature
  // This should NOT be detected as a validation override
  trait UnrelatedValidate:
    def validate(s: String): Boolean = s.nonEmpty // Different signature!

  type NoValidation = NoValidation.Type
  object NoValidation extends Newtype[Int] with UnrelatedValidate

  val spec = suite("InheritedValidateSpec")(
    suite("Direct validate (control)")(
      test("compile-time validation works for valid input") {
        val x: DirectPositive = DirectPositive(42)
        assertTrue(x.unwrap == 42)
      },
      test("compile-time validation fails for invalid input") {
        val errors = typeCheckErrors("""DirectPositive(-1)""")
        assertTrue(errors.nonEmpty)
      }
    ),
    suite("Inherited validate (the bug?)")(
      test("compile-time validation works for valid input") {
        val x: InheritedPositive = InheritedPositive(42)
        assertTrue(x.unwrap == 42)
      },
      test("compile-time validation SHOULD fail for invalid input") {
        // If this passes with empty errors, the bug is real!
        val errors = typeCheckErrors("""InheritedPositive(-1)""")
        assertTrue(errors.nonEmpty)
      }
    ),
    suite("Runtime make() behavior")(
      test("direct make rejects invalid") {
        assertTrue(DirectPositive.make(-1).isLeft)
      },
      test("inherited make rejects invalid") {
        assertTrue(InheritedPositive.make(-1).isLeft)
      }
    ),
    suite("Unrelated validate method (edge case)")(
      test("unrelated validate(String) does NOT trigger compile-time validation") {
        // NoValidation has a validate(String) method, but NOT validate(Int)
        // So any Int should be accepted at compile-time (no validation)
        val x: NoValidation = NoValidation(-999)
        assertTrue(x.unwrap == -999)
      },
      test("unrelated validate(String) does NOT trigger runtime validation") {
        // Runtime make should also accept any Int
        assertTrue(NoValidation.make(-999).isRight)
      }
    ),
    suite("Error message quality for inherited validation")(
      test("error message shows the OBJECT name, not the trait name") {
        val errors = typeCheckErrors("""InheritedPositive(-1)""")
        val msg    = errors.head.message
        // Should say "InheritedPositive", not "PositiveIntValidation"
        assertTrue(msg.contains("InheritedPositive"))
      },
      test("error message shows the validation source code") {
        val errors = typeCheckErrors("""InheritedPositive(-1)""")
        val msg    = errors.head.message
        // Should show the validation logic from the trait
        assertTrue(msg.contains("n > 0") || msg.contains("INVALID"))
      },
      test("direct validation error message for comparison") {
        val errors = typeCheckErrors("""DirectPositive(-1)""")
        val msg    = errors.head.message
        assertTrue(msg.contains("DirectPositive"))
      }
    )
  )
