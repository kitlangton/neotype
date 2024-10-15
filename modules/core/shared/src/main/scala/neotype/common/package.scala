package neotype.common

import neotype.*

type NonEmptyString = NonEmptyString.Type
object NonEmptyString extends Subtype[String]:
  override inline def validate(input: String): Boolean =
    input.nonEmpty
