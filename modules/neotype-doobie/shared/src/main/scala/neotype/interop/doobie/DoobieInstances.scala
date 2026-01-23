package neotype.interop.doobie

import cats.Show
import doobie.*
import neotype.*

import scala.annotation.unused
import scala.reflect.ClassTag

// Helps resolve Get instances for stacked Newtypes
export neotype.interop.cats.show

given get[A, B](using nt: WrappedType[A, B], get: Get[A], show: Show[A]): Get[B] =
  get.temap(nt.make(_))

given put[A, B](using nt: WrappedType[A, B], put: Put[A]): Put[B] =
  nt.unsafeMakeF(put)

given read[A, B](using wrappedType: WrappedType[A, B], read: Read[A]): Read[B] =
   read.map(value => wrappedType.makeOrThrow(value))

given write[A, B](using wrappedType: WrappedType[A, B], write: Write[A]): Write[B] =
  write.contramap(wrappedType.unwrap)

given arrayGet[A, B: ClassTag](using nt: WrappedType[A, B], get: Get[Array[A]], show: Show[Array[A]]): Get[Array[B]] =
  get.temap { arr =>
    arr.foldRight[Either[String, Array[B]]](Right(Array.empty[B])) { (elem, acc) =>
      for
        xs <- acc
        x  <- nt.make(elem)
      yield xs :+ x
    }
  }

given arrayPut[A, B](using @unused nt: WrappedType[A, B], put: Put[Array[A]]): Put[Array[B]] =
  put.asInstanceOf[Put[Array[B]]]

given [A](using show: Show[A]): Show[Array[A]] =
  Show.show(arr => arr.map(show.show).mkString("[", ", ", "]"))
