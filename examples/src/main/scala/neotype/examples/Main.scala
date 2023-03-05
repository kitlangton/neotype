package neotype.examples

import neotype.*

object Main extends App:
  Email("kit@gmail.com")              // OK
  FourSeasons("Spring")               // OK
  NonEmptyString("Good")              // OK
  FiveElements("REALLY LONG ELEMENT") // OK
  PositiveIntList(List(5, 10))
