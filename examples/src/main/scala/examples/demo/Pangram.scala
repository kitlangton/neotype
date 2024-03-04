package examples.demo

import neotype.*

type Pangram = Pangram.Type
object Pangram extends Newtype[String]:
  override inline def validate(input: String) =
    val missingLetters = ('a' to 'z').filterNot(input.toLowerCase.contains)
    if missingLetters.isEmpty then true
    else s"""
    |A pangram must contain every letter of the alphabet at least once."
    |Missing letters: ${missingLetters.mkString(", ")}
    """.stripMargin.trim
