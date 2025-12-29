package neotype.interop.quill

import io.getquill.MappedEncoding
import neotype.*

inline given [A, B](using nt: WrappedType[A, B]): MappedEncoding[A, B] =
  MappedEncoding { (a: A) =>
    nt.make(a).fold(e => throw new Error(s"Failed to create newtype: $e"), identity)
  }

given [A, B](using nt: WrappedType[A, B]): MappedEncoding[B, A] =
  MappedEncoding { (b: B) =>
    nt.unwrap(b)
  }
