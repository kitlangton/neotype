package examples.demo

import neotype.*

object PangramColdStorage:
  def store(pangram: String): Unit =
    println(s"\nPREPARING TO STORE PANGRAM: ${pangram.toString.yellow}".blue)
    if !isPangram(pangram) then
      throw IllegalArgumentException(
        scala.Console.RED + s"NON-PANGRAM DETECTED! You died of dysentery." + scala.Console.RESET
      )
    else println(s"PANGRAM STORED. ${Gratitude.random}\n".blue)
