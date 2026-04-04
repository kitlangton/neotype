package neotype.interop.zioconfig

import neotype.*
import zio.Chunk
import zio.Config
import zio.config.magnolia.*

given validated[A, B](using nt: ValidatedWrappedType[A, B], config: DeriveConfig[A]): DeriveConfig[B] =
  config.mapOrFail(nt.make(_).left.map(e => Config.Error.InvalidData(Chunk.empty, e)))

given simple[A, B](using nt: SimpleWrappedType[A, B], config: DeriveConfig[A]): DeriveConfig[B] =
  nt.unsafeMakeF(config)

given validatedMap[A, B, V](using
    nt: ValidatedWrappedType[A, B],
    mapConfig: DeriveConfig[Map[A, V]]
): DeriveConfig[Map[B, V]] =
  mapConfig.mapOrFail(avMap =>
    avMap.foldLeft[Either[Config.Error, Map[B, V]]](Right(Map.empty)) { case (acc, (keyA, value)) =>
      for
        map <- acc
        b <- nt.make(keyA)
               .left
               .map(e => Config.Error.InvalidData(Chunk.empty, message = e))
      yield map + (b -> value)
    }
  )

given simpleMap[A, B, V](using
    nt: SimpleWrappedType[A, B],
    mapConfig: DeriveConfig[Map[A, V]]
): DeriveConfig[Map[B, V]] =
  mapConfig.map(avMap => avMap.map((a, v) => nt.unsafeMake(a) -> v))
