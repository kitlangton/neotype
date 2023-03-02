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
  def validate(value: A): Boolean

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

abstract class Newtype[A](using fromExpr: FromExpr[A]) extends ValidatedWrapper[A]:
  self =>
  opaque type Type = A

  inline def apply(inline value: A): Type =
    ${ Macros.applyImpl[A, Type, self.type]('value, '{ validate(_) }, 'failureMessage) }

  inline def applyAll(inline values: A*): List[Type] =
    ${ Macros.applyAllImpl[A, Type, self.type]('values, 'self) }

  def make(value: A): Either[String, Type] =
    if validate(value) then Right(value)
    else Left(failureMessage)

  extension (inline value: Type) //
    inline def unwrap: A = value

object Newtype:
  type WithType[A, B] = Newtype[A] { type Type = B }

  trait Simple[A] extends Wrapper[A]:
    opaque type Type = A

    inline def apply(inline value: A): Type = value
    extension (inline value: Type) //
      inline def unwrap: A = value

    inline def applyF[F[_]](inline value: F[A]): F[Type] = value

  object Simple:
    type WithType[A, B] = Newtype.Simple[A] { type Type = B }

abstract class Subtype[A](using fromExpr: FromExpr[A]) extends ValidatedWrapper[A]:
  self =>
  opaque type Type <: A = A

  inline def apply(inline value: A): Type =
    ${ Macros.applyImpl[A, Type, self.type]('value, '{ validate(_) }, 'failureMessage) }

  def make(value: A): Either[String, Type] =
    if validate(value) then Right(value)
    else Left(failureMessage)

  inline def cast(inline value: Type): A              = value
  inline def castF[F[_]](inline value: F[Type]): F[A] = value

object Subtype:
  type WithType[A, B <: A] = Subtype[A] { type Type = B }

  trait Simple[A] extends Wrapper[A]:
    opaque type Type <: A = A

    inline def apply(inline value: A): Type = value

    extension (inline value: Type) //
      inline def unwrap: A = value

    inline def applyF[F[_]](inline value: F[A]): F[Type] = value
    inline def cast(inline value: A): Type               = value
    inline def castF[F[_]](inline value: F[A]): F[Type]  = value

  object Simple:
    type WithType[A, B <: A] = Subtype.Simple[A] { type Type = B }
