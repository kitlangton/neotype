package examples.demo

import neotype.*
import zio.json.*

object JsonColdStorage:
  def store[A: JsonEncoder](a: A): Unit =
    val jsonified = a.toJsonPretty
    println(s"\nPREPARING TO STORE JSON: ${jsonified.yellow}".blue)
    println(s"JSON STORED. ${Gratitude.random}\n".blue)