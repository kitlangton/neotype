package neotype.interop.tethys

import neotype.*
import tethys.JsonReader
import tethys.JsonWriter

// NewType
given [A, B](using newType: Newtype.WithType[A, B], reader: JsonReader[A]): JsonReader[B] =
  reader.map { a =>
    newType.make(a) match
      case Left(cause)  => throw new Throwable(cause)
      case Right(value) => value
  }

given [A, B](using newType: Newtype.WithType[A, B], writer: JsonWriter[A]): JsonWriter[B] =
  writer.contramap(_.unwrap)

// SubType
given [A, B <: A](using newType: Subtype.WithType[A, B], reader: JsonReader[A]): JsonReader[B] =
  reader.map { a =>
    newType.make(a) match
      case Left(cause)  => throw new Throwable(cause)
      case Right(value) => value
  }

given [A, B <: A](using newType: Subtype.WithType[A, B], writer: JsonWriter[A]): JsonWriter[B] =
  writer.contramap(identity)
