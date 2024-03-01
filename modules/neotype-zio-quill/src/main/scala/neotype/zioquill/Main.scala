package neotype.zioquill

import neotype.*
import io.getquill
import io.getquill.MappedEncoding

// Newtype
inline given [A, B](using newType: Newtype.WithType[A, B]): MappedEncoding[A, B] =
  MappedEncoding { (a: A) =>
    newType.make(a).fold(e => throw new Error(s"Failed to create newtype: $e"), identity)
  }

given [A, B](using newType: Newtype.WithType[A, B]): MappedEncoding[B, A] =
  MappedEncoding { (b: B) =>
    newType.unwrap(b)
  }

// Newtype.Simple

given [A, B](using newType: Newtype.Simple.WithType[A, B]): MappedEncoding[A, B] =
  MappedEncoding { (a: A) =>
    newType.apply(a)
  }

given [A, B](using newType: Newtype.Simple.WithType[A, B]): MappedEncoding[B, A] =
  MappedEncoding { (b: B) =>
    newType.unwrap(b)
  }

// Subtype

inline given [A, B <: A](using newType: Subtype.WithType[A, B]): MappedEncoding[A, B] =
  MappedEncoding { (a: A) =>
    newType.make(a).fold(e => throw new Error(s"Failed to create newtype: $e"), identity)
  }

given [A, B <: A](using newType: Subtype.WithType[A, B]): MappedEncoding[B, A] =
  MappedEncoding { (b: B) =>
    b
  }

// Subtype.Simple

given [A, B <: A](using newType: Subtype.Simple.WithType[A, B]): MappedEncoding[A, B] =
  MappedEncoding { (a: A) =>
    newType.apply(a)
  }

given [A, B <: A](using newType: Subtype.Simple.WithType[A, B]): MappedEncoding[B, A] =
  MappedEncoding { (b: B) =>
    b
  }
