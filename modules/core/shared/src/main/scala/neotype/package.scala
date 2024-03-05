package neotype

import scala.compiletime.summonInline
import scala.quoted.*
import scala.quoted.runtime.StopMacroExpansion
import javax.xml.validation.Validator

sealed abstract class Neotype[A]:
  type Type

  /** Validates the input and returns a boolean or an error message.
    */
  def validate(input: A): Boolean | String = true

  /** Creates a new instance of the newtype.
    *
    * If the `validate` method is not defined for this, it will simply wrap the
    * input in the neotype.
    *
    * If `validate` is defined, it will be called at compile-time to check if
    * the input is valid. If it fails, it will raise a compile-time error. input
    * should be a literal or a constant expression, a compile-time known
    * value—otherwise, you should use the `make` method instead.
    */
  inline def apply(inline input: A): Type =
    ${ Macros.applyImpl[A, Type, this.type]('input, '{ INPUT => validate(INPUT) }) }

  /** Creates a list of newtype instances.
    *
    * This will essentially call `apply` for each value in the list, performing
    * the same compile-time validation for each element.
    *
    * If any of the elements fail validation, it will raise a compile-time
    * error. Otherwise, it will return a list of newtype instances.
    */
  inline def applyAll(inline values: A*): List[Type] =
    ${ Macros.applyAllImpl[A, Type, this.type]('values, 'this) }

  /** WARNING! Creates a new instance of the newtype without performing any
    * validation. ONLY USE THIS IF YOU KNOW WHAT YOU'RE DOING.
    *
    * This method should be used with caution, as it bypasses any validation
    * defined in the `validate` method. It is recommended to use the `apply`
    * method instead, which performs compile-time validation of the input.
    */
  inline def unsafeMake(inline input: A): Type

  /** WARNING! This casts an `F[A]` to an `F[Type]` without performing any
    * validation. ONLY USE THIS IF YOU KNOW WHAT YOU'RE DOING.
    *
    * If, for instance, you have a `List[A]` and you want to convert it to a
    * `List[Type]`, you can use this method to do so.
    */
  inline def unsafeMakeF[F[_]](inline input: F[A]): F[Type]

/** `Newtype` allows for the creation of distinct types from existing ones,
  * enabling more type-safe code by leveraging the type system to enforce
  * constraints at compile time.
  *
  * Overriding the `validate` method allows for custom validation logic to be
  * executed at compile time, ensuring that the newtype is only created with
  * valid input. If the validation fails, it will raise a compile-time error.
  *
  * Example:
  *
  * ```
  * // DEFINITION
  * object Digits extends Newtype[String]:
  *   override inline def validate(value: String) =
  *     if value.forall(_.isDigit) then true
  *     else "String must be numeric"
  *
  * // USAGE
  * Digits("123") // Compiles successfully.
  * Digits("abc") // Would fail to compile due to validation.
  * ```
  */

abstract class Newtype[A] extends Neotype[A]:
  opaque type Type = A

  transparent inline given instance: Newtype.WithType[A, Type] = this

  /** Attempts to create a new instance of the Newtype for the given input by
    * calling the `validate` method.
    *
    * If the validation fails, it will return a `Left` with an error message. If
    * the validation succeeds, it will return a `Right` with the new instance.
    */
  def make(input: A): Either[String, Type] =
    validate(input) match
      case true            => Right(input)
      case false           => Left("Validation Failed")
      case message: String => Left(message)

  inline def unwrap(inline input: Type): A = input

  inline def unsafeMake(inline input: A): Type              = input
  inline def unsafeMakeF[F[_]](inline input: F[A]): F[Type] = input

object Newtype:
  type WithType[A, B] = Newtype[A] { type Type = B }

extension [A, B](value: B)(using newtype: Newtype.WithType[A, B]) //
  /** Unwraps the newtype, returning the underlying value. */
  inline def unwrap = newtype.unwrap(value)

abstract class Subtype[A] extends Neotype[A]:
  opaque type Type <: A = A

  transparent inline given Subtype.WithType[A, Type] = this

  /** Attempts to create a new instance of the Subtype for the given input by
    * calling the `validate` method.
    *
    * If the validation fails, it will return a `Left` with an error message. If
    * the validation succeeds, it will return a `Right` with the new instance.
    */
  def make(input: A): Either[String, Type] =
    validate(input) match
      case true            => Right(input)
      case false           => Left("Validation Failed")
      case message: String => Left(message)

  inline def unsafeMake(inline input: A): Type              = input
  inline def unsafeMakeF[F[_]](inline input: F[A]): F[Type] = input

object Subtype:
  type WithType[A, B <: A] = Subtype[A] { type Type = B }
