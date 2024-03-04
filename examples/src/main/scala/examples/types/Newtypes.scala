package examples.types

import neotype.*

type Name = Name.Type
object Name extends Newtype[String]

type NonEmptyString = NonEmptyString.Type
object NonEmptyString extends Newtype[String]:
  override inline def validate(input: String): Boolean =
    input.nonEmpty

type Email = Email.Type
object Email extends Newtype[String]:
  override inline def validate(value: String): Result =
    if value.contains("@") && value.contains(".") then true else "Invalid email"

type FourSeasons = FourSeasons.Type
object FourSeasons extends Newtype[String]:
  override inline def validate(value: String): Boolean =
    val seasons = Set("Spring", "Summer", "Autumn", "Winter")
    seasons.contains(value)

type FiveElements = FiveElements.Type
object FiveElements extends Newtype[String]:
  override inline def validate(value: String): Boolean =
    value match
      case "Metal" | "Water" | "Wood" | "Fire" | "Earth" => true
      case string if string.length > 10                  => true
      case _                                             => false

type PositiveIntList = PositiveIntList.Type
object PositiveIntList extends Newtype[List[Int]]:
  override inline def validate(value: List[Int]): Boolean =
    !value.exists(_ < 0)
