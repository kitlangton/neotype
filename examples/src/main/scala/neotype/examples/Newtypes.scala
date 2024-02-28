package neotype.examples

import neotype.*

type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  inline def validate(value: String): Boolean =
    value.reverse.nonEmpty

type Email = Email.Type
given Email: Newtype[String] with

  inline def validate(value: String): Boolean =
    value.contains("@") && value.contains(".")

type FourSeasons = FourSeasons.Type
given FourSeasons: Newtype[String] with
  inline def validate(value: String): Boolean =
    val seasons = Set("Spring", "Summer", "Autumn", "Winter")
    seasons.contains(value)

type FiveElements = FiveElements.Type
given FiveElements: Newtype[String] with
  inline def validate(value: String): Boolean =
    value match
      case "Metal" | "Water" | "Wood" | "Fire" | "Earth" => true
      case string if string.length > 10                  => true
      case _                                             => false

type PositiveIntList = PositiveIntList.Type
given PositiveIntList: Newtype[List[Int]] with
  inline def validate(value: List[Int]): Boolean =
    value.filter(_ < 0).isEmpty
