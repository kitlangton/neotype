package neotype.examples

import neotype.*

object Main extends App:
  // These will all compile.
  Email("kit@gmail.com")       // OK
  FourSeasons("Spring")        // OK
  FiveElements("Fire")         // OK
  NonEmptyString("Good")       // OK
  PositiveIntList(List(5, 10)) // OK

  // Uncomment out the following lines one at a time to see some fun compile errors.
  // Email("kitgmail.com")        // BAD
  // FourSeasons("Splinter")      // BAD
  // FiveElements("Cheese")       // BAD
  // NonEmptyString("")           // BAD
  // PositiveIntList(List(5, -5)) // BAD
