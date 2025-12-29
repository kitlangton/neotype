package neotype.common

import neotype.*

type UrlString = UrlString.Type
object UrlString extends Subtype[String]:
  override inline def validate(input: String): Boolean | String =
    if scala.util.Try(new java.net.URI(input).toURL).isFailure then "Must be a valid URL" else true
