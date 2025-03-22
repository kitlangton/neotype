package neotype.interop.pureconfig

import neotype.*
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

import scala.reflect.ClassTag

// Newtype

// Read inner type into newtype
given [A, B](using newtype: Newtype.WithType[A, B], configReader: ConfigReader[A], tag: ClassTag[B]): ConfigReader[B] =
  configReader.emap(a => newtype.make(a).left.map(error => CannotConvert(a.toString, tag.toString, error)))

// Subtype

// Read inner type into newtype
given [A, B <: A](using
    subtype: Subtype.WithType[A, B],
    configReader: ConfigReader[A],
    tag: ClassTag[B]
): ConfigReader[B] =
  configReader.emap(a => subtype.make(a).left.map(error => CannotConvert(a.toString, tag.toString, error)))
