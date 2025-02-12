package neotype.interop.ciris
import ciris.*
import neotype.*

given validatedNewType[A, B](using
    newType: Newtype.WithType[A, B],
    configDecoder: ConfigDecoder[A, A],
    ev: IsValidatedType[newType.type]
): ConfigDecoder[A, B] =
  configDecoder.mapEither: (_, value) =>
    newType.make(value).left.map(e => ConfigError(e))

given simpleNewType[A, B](using
    newType: Newtype.WithType[A, B],
    configDecoder: ConfigDecoder[A, A],
    ev: IsSimpleType[newType.type]
): ConfigDecoder[A, B] =
  configDecoder.map(newType.unsafeMake)

given validatedSubtype[A, B <: A](using
    subType: Subtype.WithType[A, B],
    configDecoder: ConfigDecoder[A, A],
    ev: IsValidatedType[subType.type]
): ConfigDecoder[A, B] =
  configDecoder.mapEither: (_, value) =>
    subType.make(value).left.map(e => ConfigError(e))

given simpleSubtype[A, B <: A](using
    subType: Subtype.WithType[A, B],
    configDecoder: ConfigDecoder[A, A],
    ev: IsSimpleType[subType.type]
): ConfigDecoder[A, B] =
  configDecoder.map(subType.unsafeMake)
