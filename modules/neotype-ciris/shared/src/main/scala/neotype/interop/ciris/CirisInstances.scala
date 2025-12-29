package neotype.interop.ciris
import ciris.*
import neotype.*

given validated[A, B](using
    nt: ValidatedWrappedType[A, B],
    configDecoder: ConfigDecoder[A, A]
): ConfigDecoder[A, B] =
  configDecoder.mapEither: (_, value) =>
    nt.make(value).left.map(e => ConfigError(e))

given simple[A, B](using
    nt: SimpleWrappedType[A, B],
    configDecoder: ConfigDecoder[A, A]
): ConfigDecoder[A, B] =
  configDecoder.map(value => nt.unsafeMake(value))
