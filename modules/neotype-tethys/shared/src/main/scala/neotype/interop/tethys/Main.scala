package neotype.interop.tethys

import neotype.*
import tethys.JsonReader
import tethys.JsonWriter
import tethys.readers.ReaderError

// NewType
given [A, B](using newType: Newtype.WithType[A, B], reader: JsonReader[A]): JsonReader[B] =
  reader.mapWithField { implicit fieldName => a =>
    newType.make(a).fold(ReaderError.wrongJson(_), identity)
  }

given [A, B](using newType: Newtype.WithType[A, B], writer: JsonWriter[A]): JsonWriter[B] =
  writer.contramap(_.unwrap)

// SubType
given [A, B <: A](using newType: Subtype.WithType[A, B], reader: JsonReader[A]): JsonReader[B] =
  reader.mapWithField { implicit fieldName => a =>
    newType.make(a).fold(ReaderError.wrongJson(_), identity)
  }

given [A, B <: A](using newType: Subtype.WithType[A, B], writer: JsonWriter[A]): JsonWriter[B] =
  writer.contramap(identity)
