package neotype.interop.scanamo

import neotype.*
import org.scanamo.*

import scala.reflect.ClassTag

// Newtype

given [A, B](using nt: Newtype.WithType[A, B], df: DynamoFormat[A]): DynamoFormat[B] = new DynamoFormat[B]:
  def read(av: DynamoValue): Either[DynamoReadError, B] =
    df.read(av).flatMap(a => nt.make(a).left.map(err => TypeCoercionError(new RuntimeException(err))))

  def write(t: B): DynamoValue = df.write(nt.unwrap(t))
