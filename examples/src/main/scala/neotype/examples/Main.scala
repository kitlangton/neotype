package neotype.examples

import neotype.*

object Main extends App:
  FourSeasons("Spring")               // OK
  FiveElements("REALLY LONG ELEMENT") // OK
  FiveElements("HOh")                 // OK
  NonEmptyString("Good")              // OK
