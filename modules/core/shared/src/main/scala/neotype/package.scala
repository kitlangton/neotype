package neotype

import scala.compiletime.summonInline
import scala.quoted.*
import scala.quoted.runtime.StopMacroExpansion

sealed trait ValidatedWrapper[A]:
  type Type

  def validate(input: A): Boolean = true
  def failureMessage: String      = "Validation Failed"

  inline def apply(inline input: A): Type =
    ${ Macros.applyImpl[A, Type, this.type]('input, 'validate, 'failureMessage) }

  inline def applyAll(inline values: A*): List[Type] =
    ${ Macros.applyAllImpl[A, Type, this.type]('values, 'this) }

  inline def unsafeMake(inline input: A): Type
  inline def unsafeMakeF[F[_]](inline input: F[A]): F[Type]

  trait ValidateEvidence
  given ValidateEvidence = new ValidateEvidence {}
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
  opaque type Type = A

  transparent inline given instance: Newtype.WithType[A, Type] = this

  def make(input: A): Either[String, Type] =
    if validate(input) then Right(input)
    else Left(failureMessage)

  extension (inline input: Type) //
    inline def unwrap: A = input

  inline def unsafeMake(inline input: A): Type              = input
  inline def unsafeMakeF[F[_]](inline input: F[A]): F[Type] = input

object Newtype:
  type WithType[A, B] = Newtype[A] { type Type = B }

abstract class Subtype[A] extends ValidatedWrapper[A]:
  opaque type Type <: A = A

  transparent inline given Subtype.WithType[A, Type] = this

  def make(input: A): Either[String, Type] =
    if validate(input) then Right(input)
    else Left(failureMessage)

  inline def unsafeMake(inline input: A): Type              = input
  inline def unsafeMakeF[F[_]](inline input: F[A]): F[Type] = input

object Subtype:
  type WithType[A, B <: A] = Subtype[A] { type Type = B }
