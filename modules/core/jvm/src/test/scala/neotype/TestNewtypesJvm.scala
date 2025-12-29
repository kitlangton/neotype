package neotype

type UrlStringNewtype = UrlStringNewtype.Type
object UrlStringNewtype extends Newtype[String]:
  override inline def validate(input: String) =
    if scala.util.Try(new java.net.URI(input).toURL).isFailure then "Must be a valid URL" else true
