package neotype

object Use extends App:
  EqualityParsingNewtype("secret string")
  LessThanParsingNewtype("sectret string")

//given ArithmeticNewtype: Newtype[Double] with
//  inline def validate(input: Double) =
//    val y = input
//    y - 10 == 0
//
//object Testing extends App:
//  val int = 10.5
//  ArithmeticNewtype(int) // ok
