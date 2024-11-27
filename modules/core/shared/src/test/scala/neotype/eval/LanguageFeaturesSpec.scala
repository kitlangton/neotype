package neotype.eval

import neotype.TestMacros.*
import zio.test.*

object LanguageFeaturesSpec extends ZIOSpecDefault:
  val spec =
    suite("LanguageFeaturesSpec")(
      evalTests.map { case (actual, expected) =>
        test(s"eval($actual) == $expected") {
          assertTrue(actual == expected)
        }
      }
    )

  inline def wrap[A](a: A): A = a

  case class Person(name: String, age: Int)

  lazy val evalTests = List(
    // Tuple operations
    eval((1, 2)._1)    -> 1,
    eval((1, 2)._2)    -> 2,
    eval((1, 2, 3)._3) -> 3,

    // Union type operations
    eval("A": Int | String) -> "A",
    eval(1: Int | String)   -> 1,

    // Lambda expressions
    eval {
      def f(x: Int) = x + 1
      f(10)
    } -> 11,
    eval {
      List(0, 1, 2, 3).map(_ * 2)
    }                                         -> List(0, 2, 4, 6),
    eval(List(0, 1, 2, 3).filter(a => a > 1)) -> List(2, 3),

    // Custom type operations
    eval {
      val x = Person("Hello", 3)
      x.name * x.age
    } -> "HelloHelloHello",

    // Control flow
    eval {
      val x = 10
      if x > 5 then x else "hello"
    } -> 10
  )
