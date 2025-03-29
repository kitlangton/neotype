package neotype.interop.scanamo

import neotype.*
import org.scanamo.*

// Newtype

given [A, B](using nt: Newtype.WithType[A, B], df: DynamoFormat[A]): DynamoFormat[B] = new DynamoFormat[B]:
  def read(av: DynamoValue): Either[DynamoReadError, B] =
    df.read(av).flatMap(a => nt.make(a).left.map(err => TypeCoercionError(NeotypeValidationError(err))))

  def write(t: B): DynamoValue = df.write(nt.unwrap(t))

// Subtype

given [A, B <: A](using st: Subtype.WithType[A, B], df: DynamoFormat[A]): DynamoFormat[B] = new DynamoFormat[B]:
  def read(av: DynamoValue): Either[DynamoReadError, B] =
    df.read(av).flatMap(a => st.make(a).left.map(err => TypeCoercionError(NeotypeValidationError(err))))

  def write(t: B): DynamoValue = df.write(t)

// Support types

final case class NeotypeValidationError(message: String) extends Throwable:
  override def toString: String = message
