package comptime

import scala.quoted.*

object ScalaAstBridge:
  def termToIR[Q <: Quotes](using quotes: Q)(term0: quotes.reflect.Term): TermIR =
    ScalaAstBridgeTerms.termToIR(term0)
