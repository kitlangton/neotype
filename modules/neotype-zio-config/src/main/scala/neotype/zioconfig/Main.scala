package neotype.zioconfig

import neotype.*
import zio.config.*
import zio.config.magnolia.*

// Newtype
given [A, B](using newtype: Newtype.WithType[A, B], descriptor: Descriptor[A]): Descriptor[B] =
  Descriptor.from(descriptor.desc.mapEither(newtype.make))

// Newtype.Simple
given [A, B](using newtype: Newtype.Simple.WithType[A, B], descriptor: Descriptor[A]): Descriptor[B] =
  Descriptor.from(descriptor.desc.map(newtype.apply(_)))

// Subtype
given [A, B <: A](using subtype: Subtype.WithType[A, B], descriptor: Descriptor[A]): Descriptor[B] =
  Descriptor.from(descriptor.desc.mapEither(subtype.make))

// Subtype.Simple
given [A, B <: A](using subtype: Subtype.Simple.WithType[A, B], descriptor: Descriptor[A]): Descriptor[B] =
  Descriptor.from(descriptor.desc.map(subtype.apply(_)))
