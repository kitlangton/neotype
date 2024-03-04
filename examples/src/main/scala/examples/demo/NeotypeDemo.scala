package examples.demo

import neotype.*

// NEOTYPE NEOTYPE NEOTYPE NEOTYPE NEOTYPE
// |  \ |  __|   _ \ __ __| \ \  / _ \ __|
// | .  |  _|   (   |   |    \  /  __/ _|
// |_|\_| ___| \___/   _|     _|  _|  ___|
// NEOTYPE NEOTYPE NEOTYPE NEOTYPE NEOTYPE
//
// ****************************************
// * NEWTYPES + REFINED TYPES FOR SCALA 3 *
// ****************************************
//
// "The Combination Pizza Hut and Taco Bell
//             of the Scala 3 type system."
//
//                             — Heraclitus
//
// EXAMPLE 1: PANGRAMS
// --
// A pangram is a sentence that contains
// every letter of the alphabet at least once.

def isPangram(s: String): Boolean =
  ('a' to 'z').forall(s.toLowerCase.contains)

object Pangrams extends App:

  val sentence1 = "Cozy sphinx waves quart jug of bad milk"
  val sentence2 = "Vex quest wizard, judge my backflop hand"
  val sentence3 = "Joaquin Phoenix DVD was cursed by Gylfi"

  // PangramColdStorage.store(sentence1)
  // PangramColdStorage.store(sentence2)
  // PangramColdStorage.store(sentence3)
