package neotype

import scala.quoted.*

sealed abstract class TypeWrapper[A]:
  type Type

  /** Validates the input and returns a boolean or an error message.
    *
    * TODO: Migrate to Either[String, A] API for validation:
    *   - Either allows transformation/normalization (e.g., trimming,
    *     lowercasing)
    *   - Right(value) = valid (possibly transformed)
    *   - Left(message) = invalid with error message
    *   - Deprecation path: add validateE alongside validate, migrate, then
    *     deprecate
    *   - Example: Email.apply(" FOO@BAR.COM ") could normalize to "foo@bar.com"
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

  protected final def validateInput(input: A): Either[String, A] =
    validate(input) match
      case true            => Right(input)
      case false           => Left("Validation Failed")
      case message: String => Left(message)

  protected final def validatedOrThrow(input: A): A =
    validateInput(input) match
      case Right(value) => value
      case Left(error)  => throw new IllegalArgumentException(error)

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

/** Helper for compile-time parsing into a value without wrapping it. */
abstract class ParsedFrom[In, Out]:
  /** Parse a value from input.
    *
    * Should be pure and comptime-friendly if you want compile-time parsing.
    */
  inline def parse(inline input: In): Either[String, Out]

  /** Parse at compile time when input is known; fail compilation on Left. */
  inline def apply(inline input: In): Out =
    ${ Macros.parseApplyImpl[In, Out, this.type]('input, '{ INPUT => parse(INPUT) }) }

  /** Parse at runtime, returning Either. */
  inline def make(input: In): Either[String, Out] =
    parse(input)

/** Convenience for String-based parsing. */
abstract class Parsed[Out] extends ParsedFrom[String, Out]

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
  * ```scala
  * // DEFINITION
  * type Digits = Digits.Type
  * object Digits extends Newtype[String]:
  *   override inline def validate(value: String) =
  *     value.forall(_.isDigit)
  *
  * // COMPILE-TIME VALIDATION
  * Digits("123") // Compiles successfully.
  * Digits("abc") // Would fail to compile due to validation.
  *
  * // RUN-TIME VALIDATION
  * val input = "123"
  * Digits.make(input) // Right(Digits("123"))
  *
  * val bad = "abc"
  * Digits.make(bad) // Left("String must be numeric")
  * ```
  */
abstract class Newtype[A] extends TypeWrapper[A]:
  opaque type Type = A

  transparent inline given instance: Newtype.WithType[A, Type] = this

  /** Attempts to create a new instance of the Newtype for the given input by
    * calling the `validate` method.
    *
    * If the validation fails, it will return a `Left` with an error message. If
    * the validation succeeds, it will return a `Right` with the new instance.
    */
  final def make(input: A): Either[String, Type] =
    validateInput(input).map(unsafeMake)

  /** Attempts to create a new instance of the Newtype for the given input by
    * calling the `validate` method, throwing an exception on failure.
    *
    * Unlike `unsafeMake`, this method still performs validation. It is useful
    * in tests or scripts where you don't want to handle `Either` but still want
    * validation to run.
    *
    * @throws IllegalArgumentException
    *   if validation fails
    */
  final def makeOrThrow(input: A): Type =
    unsafeMake(validatedOrThrow(input))

  inline def unwrap(inline input: Type): A = input

  inline def unsafeMake(inline input: A): Type              = input
  inline def unsafeMakeF[F[_]](inline input: F[A]): F[Type] = input

object Newtype:
  type WithType[A, B] = Newtype[A] { type Type = B }

extension [A, B](value: B)(using
    newtype: Newtype.WithType[A, B]
) //
  /** Unwraps the newtype, returning the underlying value. */
  inline def unwrap = newtype.unwrap(value)

abstract class Subtype[A] extends TypeWrapper[A]:
  opaque type Type <: A = A

  transparent inline given Subtype.WithType[A, Type] = this

  /** Attempts to create a new instance of the Subtype for the given input by
    * calling the `validate` method.
    *
    * If the validation fails, it will return a `Left` with an error message. If
    * the validation succeeds, it will return a `Right` with the new instance.
    */
  final def make(input: A): Either[String, Type] =
    validateInput(input).map(unsafeMake)

  /** Attempts to create a new instance of the Subtype for the given input by
    * calling the `validate` method, throwing an exception on failure.
    *
    * Unlike `unsafeMake`, this method still performs validation. It is useful
    * in tests or scripts where you don't want to handle `Either` but still want
    * validation to run.
    *
    * @throws IllegalArgumentException
    *   if validation fails
    */
  final def makeOrThrow(input: A): Type =
    unsafeMake(validatedOrThrow(input))

  inline def unsafeMake(inline input: A): Type              = input
  inline def unsafeMakeF[F[_]](inline input: F[A]): F[Type] = input

object Subtype:
  type WithType[A, B <: A] = Subtype[A] { type Type = B }

/** Typeclass for both Newtype and Subtype wrappers. */
trait WrappedType[Underlying, Wrapped]:
  type Wrapper <: TypeWrapper[Underlying] { type Type = Wrapped }
  def unwrap(wrapped: Wrapped): Underlying
  def make(underlying: Underlying): Either[String, Wrapped]
  inline def unsafeMake(inline underlying: Underlying): Wrapped = underlying.asInstanceOf[Wrapped]
  inline def unsafeMakeF[F[_]](inline underlying: F[Underlying]): F[Wrapped] =
    underlying.asInstanceOf[F[Wrapped]]

object WrappedType:
  inline def apply[A, B](using nt: WrappedType[A, B]): WrappedType[A, B] = nt

  given newtypeWrappedType[A, B](using nt: Newtype.WithType[A, B]): WrappedType[A, B] with
    type Wrapper = nt.type
    inline def unwrap(wrapped: B): A                  = nt.unwrap(wrapped)
    inline def make(underlying: A): Either[String, B] = nt.make(underlying)

  given subtypeWrappedType[A, B <: A](using st: Subtype.WithType[A, B]): WrappedType[A, B] with
    type Wrapper = st.type
    inline def unwrap(wrapped: B): A                  = wrapped
    inline def make(underlying: A): Either[String, B] = st.make(underlying)

trait SimpleWrappedType[Underlying, Wrapped] extends WrappedType[Underlying, Wrapped]

object SimpleWrappedType:
  inline def apply[A, B](using nt: SimpleWrappedType[A, B]): SimpleWrappedType[A, B] = nt

  given newtypeSimple[A, B](using nt: Newtype.WithType[A, B], ev: IsSimpleType[nt.type]): SimpleWrappedType[A, B] with
    type Wrapper = nt.type
    inline def unwrap(wrapped: B): A                  = nt.unwrap(wrapped)
    inline def make(underlying: A): Either[String, B] = nt.make(underlying)

  given subtypeSimple[A, B <: A](using st: Subtype.WithType[A, B], ev: IsSimpleType[st.type]): SimpleWrappedType[A, B]
  with
    type Wrapper = st.type
    inline def unwrap(wrapped: B): A                  = wrapped
    inline def make(underlying: A): Either[String, B] = st.make(underlying)

trait ValidatedWrappedType[Underlying, Wrapped] extends WrappedType[Underlying, Wrapped]

object ValidatedWrappedType:
  inline def apply[A, B](using nt: ValidatedWrappedType[A, B]): ValidatedWrappedType[A, B] = nt

  given newtypeValidated[A, B](using
      nt: Newtype.WithType[A, B],
      ev: IsValidatedType[nt.type]
  ): ValidatedWrappedType[A, B] with
    type Wrapper = nt.type
    inline def unwrap(wrapped: B): A                  = nt.unwrap(wrapped)
    inline def make(underlying: A): Either[String, B] = nt.make(underlying)

  given subtypeValidated[A, B <: A](using
      st: Subtype.WithType[A, B],
      ev: IsValidatedType[st.type]
  ): ValidatedWrappedType[A, B] with
    type Wrapper = st.type
    inline def unwrap(wrapped: B): A                  = wrapped
    inline def make(underlying: A): Either[String, B] = st.make(underlying)

/** Typeclass for wrapping an underlying value with validation. */
trait Wrappable[Underlying, Wrapped]:
  def wrap(underlying: Underlying): Either[String, Wrapped]

object Wrappable:
  inline def apply[U, W](using w: Wrappable[U, W]): Wrappable[U, W] = w

  given newtypeWrappable[A, B](using nt: Newtype.WithType[A, B]): Wrappable[A, B] =
    (underlying: A) => nt.make(underlying)

  given subtypeWrappable[A, B <: A](using st: Subtype.WithType[A, B]): Wrappable[A, B] =
    (underlying: A) => st.make(underlying)
