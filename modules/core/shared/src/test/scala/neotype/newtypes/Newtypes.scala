package neotype.newtypes

import neotype.*

type NonEmptyString = NonEmptyString.Type
object NonEmptyString extends Subtype[String]:

  override inline def validate(value: String): Boolean =
    value.nonEmpty

  override inline def failureMessage = "String must not be empty"

@main
def testNonEmptyString(): Unit =
  val result = NonEmptyString("hello")
  println(result)

  // val result2 = NonEmptyString("")
  // println(result2)
