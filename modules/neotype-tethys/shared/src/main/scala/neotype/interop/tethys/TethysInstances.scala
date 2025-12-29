package neotype.interop.tethys

import neotype.*
import tethys.JsonReader
import tethys.JsonWriter
import tethys.readers.ReaderError

given [A, B](using nt: WrappedType[A, B], reader: JsonReader[A]): JsonReader[B] =
  reader.mapWithField { implicit fieldName => a =>
    nt.make(a).fold(ReaderError.wrongJson(_), identity)
  }

given [A, B](using nt: WrappedType[A, B], writer: JsonWriter[A]): JsonWriter[B] =
  writer.contramap(nt.unwrap)
