package neotype.interop.doobie

import cats.Show
import cats.implicits.*
import doobie.*
import neotype.*

import scala.reflect.ClassTag

// Helps resolve Get instances for stacked Newtypes
export neotype.interop.cats.{newtypeShow, subtypeShow}

/////////////
// NEWTYPE //
/////////////

given newtypeGet[A, B](using newtype: Newtype.WithType[A, B], get: Get[A], show: Show[A]): Get[B] =
  get.temap(newtype.make(_))

given newtypePut[A, B](using
    newtype: Newtype.WithType[A, B],
    put: Put[A]
): Put[B] =
  newtype.unsafeMakeF(put)

given newtypeArrayGet[A, B: ClassTag](using
    newtype: Newtype.WithType[A, B],
    get: Get[Array[A]],
    show: Show[Array[A]]
): Get[Array[B]] =
  get.temap { arr =>
    arr.foldRight[Either[String, Array[B]]](Right(Array.empty[B])) { (elem, acc) =>
      for
        xs <- acc
        x  <- newtype.make(elem)
      yield xs :+ x
    }
  }

given newtypeArrayPut[A, B](using newtype: Newtype.WithType[A, B], put: Put[Array[A]]): Put[Array[B]] =
  put.asInstanceOf[Put[Array[B]]]

given [A](using show: Show[A]): Show[Array[A]] =
  Show.show(arr => arr.map(show.show).mkString("[", ", ", "]"))

/////////////
// SUBTYPE //
/////////////

given subtypeGet[A, B <: A](using subtype: Subtype.WithType[A, B], get: Get[A], show: Show[A]): Get[B] =
  get.temap(subtype.make(_))

given subtypePut[A, B <: A](using subtype: Subtype.WithType[A, B], put: Put[A]): Put[B] =
  subtype.unsafeMakeF(put)

given subtypeArrayGet[A, B <: A: ClassTag](using
    subtype: Subtype.WithType[A, B],
    get: Get[Array[A]],
    show: Show[Array[A]]
): Get[Array[B]] =
  get.temap { arr =>
    arr.foldRight[Either[String, Array[B]]](Right(Array.empty[B])) { (elem, acc) =>
      for
        xs <- acc
        x  <- subtype.make(elem)
      yield xs :+ x
    }
  }

given subtypeArrayPut[A, B <: A](using subtype: Subtype.WithType[A, B], put: Put[Array[A]]): Put[Array[B]] =
  put.asInstanceOf[Put[Array[B]]]
