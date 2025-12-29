package neotype.interop.zioconfig

import neotype.*
import zio.Chunk
import zio.Config
import zio.config.magnolia.*

given validated[A, B](using nt: ValidatedWrappedType[A, B], config: DeriveConfig[A]): DeriveConfig[B] =
  config.mapOrFail(nt.make(_).left.map(e => Config.Error.InvalidData(Chunk.empty, e)))

given simple[A, B](using nt: SimpleWrappedType[A, B], config: DeriveConfig[A]): DeriveConfig[B] =
  nt.unsafeMakeF(config)
