package examples

import neotype.*
import neotype.ziojson.given
import zio.json.*
import types.*

import scala.util.NotGiven
import zio.json.ast.Json.Num

// Here's an example of a Newtype that wraps a String.

// 1. Optionally, define a type alias. This is purely for convenience, elsewhere.
type NumericString = NumericString.Type

// 2. Define the Newtype.
object NumericString extends Newtype[String]:

  // 3. If you want to "refine" your newtype to only accept certain values,
  //    then you can override the `validate` method.
  override inline def validate(value: String): Boolean =
    value.forall(_.isDigit)

  // 4. If you want to provide a custom error message, you can override
  //    the `failureMessage` method.
  override inline def failureMessage = "String must be numeric"

// Now, let's see how we can use this newtype.
object ConstructingYourNewtype:
  // Simply, construct your newtype with a compile-time known value.
  // This will execute your `validate` method at compile time.
  NumericString("123")

  // If we try to construct a newtype with a value that doesn't pass the `validate` method,
  // we'll get a compile error.
  // Uncomment the following line to see the error.
  // NumericString("123a")

  // Now let's see what happens when we try to construct a newtype with a runtime value.
  val userProvidedString = "123"
  // Uncomment the following line to see the error.
  // NumericString(userProvidedString)

  // There are a few solutions to this problem.
  // 1. Use the `make` method, which returns an `Either` type.
  val eitherNumericString: Either[String, NumericString] = NumericString.make(userProvidedString)
  // Right(NumericString("123"))

  val badUserProvidedString                                 = "123a"
  val eitherBadNumericString: Either[String, NumericString] = NumericString.make(badUserProvidedString)
  // Left("String must be numeric")

  // 2. Use the `unsafeMake` method, which will cast the value to the newtype without validation.
  //    Obviously, only use this if you're absolutely sure the value is valid.
  val unsafeNumericString: NumericString = NumericString.unsafeMake(userProvidedString)
  // NumericString("123")

object NewtypeExamples:
  // These values are checked at compile time.
  Name("Kit Langton")
  Email("kit@gmail.com")
  FourSeasons("Spring")
  FiveElements("Fire")
  NonEmptyString("Good")
  PositiveIntList(List(5, 10))

// Uncomment out the following lines one at a time to see some fun compile errors.
// Email("kitgmail.com")
// FourSeasons("Splinter")
// FiveElements("Cheese")
// NonEmptyString("")
// PositiveIntList(List(5, -5))

object SimpleNewtypes:
  // A "Simple" Newtype does not have a `validate` method.
  // It's parameter, therefore, doesn't have to be known at compile time.
  val nameString = "Kit Langton"
  val name       = Name(nameString)

  // Whereas a "Validated" Newtype does have a `validate` method.
  // It's parameter must be known at compile time.
  val emailString = "kit@gmail.com"
  // Try uncommenting the following line to see a compile error.
  // val email       = Email(emailString)

  // If you want to construct a "Validated" Newtype from a runtime value,
  // you must can the `make` method. This might, of course, fail the validation.
  val eitherEmail: Either[String, Email] = Email.make(emailString)

  // If you're ABSOLUTELY SURE that the value is valid, you can use `unsafeMake`.
  val unsafeEmail: Email = Email.unsafeMake(emailString)
