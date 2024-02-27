package neotype

import StringFormatting.*

import javax.swing.text.Position
import scala.compiletime.summonInline
import scala.deriving.Mirror
import scala.quoted.*
import scala.util.{Failure, Success}

trait Wrapper[A]:
  type Type

trait ValidatedWrapper[A] extends Wrapper[A]:
  self =>
  def validate(input: A): Boolean

  def failureMessage: String = "Validation Failed"

  trait ValidateEvidence
  inline given ValidateEvidence = new ValidateEvidence {}
  extension (using ValidateEvidence)(inline bool: Boolean) //
    inline def ??(message: String): Boolean = bool

  extension (using ValidateEvidence)(inline string: String) //
    inline def isUUID: Boolean  = isUUIDRegex.matches(string)
    inline def isURL: Boolean   = isURLRegex.matches(string)
    inline def isEmail: Boolean = isEmailRegex.matches(string)

private inline def isUUIDRegex = "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$".r
private inline def isURLRegex  = "^(http|https)://.*$".r
private inline def isEmailRegex =
  """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

abstract class Newtype[A] extends ValidatedWrapper[A]:
  self =>
  opaque type Type = A

  inline def apply(inline input: A): Type =
    ${ Macros.applyImpl[A, Type, self.type]('input, '{ INPUT => validate(INPUT) }, 'failureMessage) }

  inline def applyAll(inline values: A*): List[Type] =
    ${ Macros.applyAllImpl[A, Type, self.type]('values, 'self) }

  def make(input: A): Either[String, Type] =
    if validate(input) then Right(input)
    else Left(failureMessage)

  extension (inline input: Type) //
    inline def unwrap: A = input

  inline def unsafeWrapF[F[_]](inline input: F[A]): F[Type] = input
  inline def unsafe(inline input: A): Type =
    make:
        input
      .getOrElse:
        throw IllegalArgumentException:
            failureMessage

object Newtype:
  type WithType[A, B] = Newtype[A] { type Type = B }

  trait Simple[A] extends Wrapper[A]:
    opaque type Type = A

    inline def apply(inline input: A): Type = input

    extension (inline input: Type) //
      inline def unwrap: A = input

    inline def applyF[F[_]](inline input: F[A]): F[Type] = input

    inline def unsafeWrapF[F[_]](inline input: F[A]): F[Type] = input

  object Simple:
    type WithType[A, B] = Newtype.Simple[A] { type Type = B }

abstract class Subtype[A] extends ValidatedWrapper[A]:
  self =>
  opaque type Type <: A = A

  inline def apply(inline input: A): Type =
    ${ Macros.applyImpl[A, Type, self.type]('input, '{ validate(_) }, 'failureMessage) }

  def make(input: A): Either[String, Type] =
    if validate(input) then Right(input)
    else Left(failureMessage)

  inline def cast(inline input: Type): A              = input
  inline def castF[F[_]](inline input: F[Type]): F[A] = input

  inline def unsafeWrap(inline input: A): Type              = input
  inline def unsafeWrapF[F[_]](inline input: F[A]): F[Type] = input

  inline def unsafe(inline input: A): Type =
    make:
        input
      .getOrElse:
        throw IllegalArgumentException:
            failureMessage

object Subtype:
  type WithType[A, B <: A] = Subtype[A] { type Type = B }

  trait Simple[A] extends Wrapper[A]:
    opaque type Type <: A = A

    inline def apply(inline input: A): Type = input

    extension (inline input: Type) //
      inline def unwrap: A = input

    inline def applyF[F[_]](inline input: F[A]): F[Type] = input
    inline def cast(inline input: A): Type               = input
    inline def castF[F[_]](inline input: F[A]): F[Type]  = input

    inline def unsafeWrap(inline input: A): Type              = input
    inline def unsafeWrapF[F[_]](inline input: F[A]): F[Type] = input

  object Simple:
    type WithType[A, B <: A] = Subtype.Simple[A] { type Type = B }
