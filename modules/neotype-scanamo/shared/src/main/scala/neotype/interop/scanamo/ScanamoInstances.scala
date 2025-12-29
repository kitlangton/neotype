package neotype.interop.scanamo

import neotype.*
import org.scanamo.*

given [A, B](using nt: WrappedType[A, B], df: DynamoFormat[A]): DynamoFormat[B] = new DynamoFormat[B]:
  def read(av: DynamoValue): Either[DynamoReadError, B] =
    df.read(av).flatMap(a => nt.make(a).left.map(err => TypeCoercionError(NeotypeValidationError(err))))

  def write(t: B): DynamoValue = df.write(nt.unwrap(t))

// Support types

final case class NeotypeValidationError(message: String) extends Throwable:
  override def toString: String = message
