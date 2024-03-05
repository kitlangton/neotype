package neotype.test.definitions

import neotype.*

type ValidatedNewtype = ValidatedNewtype.Type
object ValidatedNewtype extends Newtype[String]:
  override inline def validate(value: String) =
    if value.nonEmpty then true else "String must not be empty"

type SimpleNewtype = SimpleNewtype.Type
object SimpleNewtype extends Newtype[Int]

type ValidatedSubtype = ValidatedSubtype.Type
object ValidatedSubtype extends Subtype[String]:
  override inline def validate(value: String) =
    if value.length > 10 then true else "String must be longer than 10 characters"

type SimpleSubtype = SimpleSubtype.Type
object SimpleSubtype extends Subtype[Int]

case class Composite(
    newtype: ValidatedNewtype,
    simpleNewtype: SimpleNewtype,
    subtype: ValidatedSubtype,
    simpleSubtype: SimpleSubtype
)

case class CompositeUnderlying(
    newtype: String,
    simpleNewtype: Int,
    subtype: String,
    simpleSubtype: Int
)
