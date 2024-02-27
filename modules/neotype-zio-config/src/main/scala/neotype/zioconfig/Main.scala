package neotype.zioconfig

import neotype.*
import zio.config.*
import zio.config.magnolia.*
import zio.Config
import zio.Chunk

// Newtype
given [A, B](using newtype: Newtype.WithType[A, B], config: DeriveConfig[A]): DeriveConfig[B] =
  config.mapOrFail(newtype.make(_).left.map(e => Config.Error.InvalidData(Chunk.empty, e)))

// Newtype.Simple
given [A, B](using newtype: Newtype.Simple.WithType[A, B], config: DeriveConfig[A]): DeriveConfig[B] =
  config.map(newtype.apply(_))

// Subtype
given [A, B <: A](using subtype: Subtype.WithType[A, B], config: DeriveConfig[A]): DeriveConfig[B] =
  config.mapOrFail(subtype.make(_).left.map(e => Config.Error.InvalidData(Chunk.empty, e)))

// Subtype.Simple
given [A, B <: A](using subtype: Subtype.Simple.WithType[A, B], config: DeriveConfig[A]): DeriveConfig[B] =
  config.map(subtype.apply(_))
