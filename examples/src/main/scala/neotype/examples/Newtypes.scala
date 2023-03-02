package neotype.examples

import neotype.*

given NonEmptyString: Newtype[String] with
  inline def validate(value: String): Boolean =
    value.nonEmpty

given FourSeasons: Newtype[String] with
  inline def validate(value: String): Boolean =
    val seasons = Set("Spring", "Summer", "Autumn", "Winter")
    seasons.contains(value)

given FiveElements: Newtype[String] with
  inline def validate(value: String): Boolean =
    value match
      case "Metal" | "Water" | "Wood" | "Fire" | "Earth" => true
      case string if string.length > 10                  => true
      case _                                             => false
