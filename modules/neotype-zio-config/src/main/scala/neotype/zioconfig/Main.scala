package neotype.zioconfig

import neotype.*
import zio.config.*
import zio.config.magnolia.*
import zio.Config
import zio.Chunk

given validatedNewtype[A, B](using
    newtype: Newtype.WithType[A, B],
    config: DeriveConfig[A],
    ev: IsValidatedType[newtype.type]
): DeriveConfig[B] =
  config.mapOrFail(newtype.make(_).left.map(e => Config.Error.InvalidData(Chunk.empty, e)))

given simpleNewtype[A, B](using
    newtype: Newtype.WithType[A, B],
    config: DeriveConfig[A],
    ev: IsSimpleType[newtype.type]
): DeriveConfig[B] =
  newtype.unsafeMakeF(config)

given validatedSubtype[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    config: DeriveConfig[A],
    ev: IsValidatedType[subtype.type]
): DeriveConfig[B] =
  config.mapOrFail(subtype.make(_).left.map(e => Config.Error.InvalidData(Chunk.empty, e)))

given simpleSubtype[A, B <: A](using
    subtype: Subtype.WithType[A, B],
    config: DeriveConfig[A],
    ev: IsSimpleType[subtype.type]
): DeriveConfig[B] =
  subtype.unsafeMakeF(config)
