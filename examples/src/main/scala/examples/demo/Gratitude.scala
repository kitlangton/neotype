package examples.demo

import neotype.*
import zio.json.*

object Gratitude:
  val grosslyExagguratedCompliments =
    Vector(
      "You are the wind beneath my wings.",
      "Thank you for your service.",
      "My children will sing songs of your bravery.",
      "The world is a better place because of you.",
      "I am nothing without you.",
      "Please don't leave.",
      "I will never forget what you have done for me."
    )

  def random: String =
    val randomIndex = scala.util.Random.between(0, grosslyExagguratedCompliments.size)
    grosslyExagguratedCompliments(randomIndex)
