package examples.demo

import neotype.*

///////////////////////////////////
// GALLERY OF COMPILATION ERRORS //
// ----------------------------- //
// ----------------------------- //
// ----------------------------- //
// ADMIT ONE -------- NO REFUNDS //
///////////////////////////////////

// --------------------------------
// 1. FORGETTING THE INLINE KEYWORD
// --------------------------------

object OverNineThousand extends Newtype[Int]:

// EXERCISE: Delete the inline keyword.
  override inline def validate(input: Int) =
    input > 9000

val bigNumber = OverNineThousand(9001)

// --------------------------------
// 2. CONSTRUCTING WITH A NON-LITERAL
// ----------------------------------

val userDefinedNumber: Int = 9001

// EXERCISE: Uncomment the following line.
// val bigNumber2             = BigNumber(userDefinedNumber)

// ------------------------------------------
// 3. CALLING ARBITRARY, USER-DEFINED METHODS
// ------------------------------------------

object PositiveNumber extends Newtype[Int]:

// EXERCISE: Delete the inline keyword.
  inline def isAGoodNumber(input: Int) = input > 0

  override inline def validate(input: Int) =
    isAGoodNumber(input)

val invalidNumber = PositiveNumber(1)
