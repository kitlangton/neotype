package neotype.interop.pureconfig

import neotype.*
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

import scala.reflect.ClassTag

given [A, B](using nt: WrappedType[A, B], configReader: ConfigReader[A], tag: ClassTag[B]): ConfigReader[B] =
  configReader.emap(a => nt.make(a).left.map(error => CannotConvert(a.toString, tag.toString, error)))
