package examples

import neotype.*
import neotype.ziojson.given
import zio.json.*
import types.*

import scala.util.NotGiven

object Main extends App:
  // These values are checked at compile time.
  Name("Kit Langton")
  Email("kit@gmail.com")
  FourSeasons("Spring")
  FiveElements("Fire")
  NonEmptyString("Good")
  PositiveIntList(List(5, 10))

// Uncomment out the following lines one at a time to see some fun compile errors.
// Email("kitgmail.com")
// FourSeasons("Splinter")
// FiveElements("Cheese")
// NonEmptyString("")
// PositiveIntList(List(5, -5))
