package neotype.eval

import neotype.TestMacros.*
import zio.test.*

object EvalSpec extends ZIOSpecDefault:
  val spec =
    suite("EvalSpec")(
      evalTests.map { case (actual, expected) =>
        test(s"eval($actual) == $expected") {
          assertTrue(actual == expected)
        }
      }
    )

inline def wrap[A](a: A): A = a

/** Tests various ways of evaluating various expressions
  */
val evalTests =
  List(
    // Numeric Expressions
    // Int
    eval(wrap(3) == 3)          -> true,
    eval(wrap(3) eq 3)          -> true,
    eval(wrap(3) != 3)          -> false,
    eval(wrap(3) ne 3)          -> false,
    eval(wrap(3) * 2)           -> 6,
    eval(wrap(1) + 1)           -> 2,
    eval(wrap(10) - 5)          -> 5,
    eval(wrap(10) / 2)          -> 5,
    eval(wrap(20) % 3)          -> 2,
    eval(wrap(20) > 3)          -> true,
    eval(wrap(20 < 3))          -> false,
    eval(wrap(20) >= 3)         -> true,
    eval(wrap(20 <= (3: Byte))) -> false,
    eval(10.0 + (5: Char))      -> 15,

    // long
    eval(wrap(3L) * 2L)   -> 6L,
    eval(wrap(1L) + 1L)   -> 2L,
    eval(wrap(10L) - 5L)  -> 5L,
    eval(wrap(10L) / 2L)  -> 5L,
    eval(wrap(20L) % 3L)  -> 2L,
    eval(wrap(20L) > 3)   -> true,
    eval(wrap(20 < 3L))   -> false,
    eval(wrap(20) >= 3)   -> true,
    eval(wrap(20.0 <= 3)) -> false,

    // double
    eval(wrap(3.0) * 2.0)  -> 6.0,
    eval(wrap(1.5) + 1.5)  -> 3.0,
    eval(wrap(10.0) - 5.5) -> 4.5,
    eval(wrap(10.0) / 2.0) -> 5.0,
    eval(wrap(20.0) % 3.0) -> 2.0,

    // string expressions
    eval(wrap("Hello, ") + "world!")                  -> "Hello, world!",
    eval("Hello, " ++ "world!")                       -> "Hello, world!",
    eval("Scala is good".toUpperCase)                 -> "SCALA IS GOOD",
    eval("SCALA IS GOOD".toLowerCase)                 -> "scala is good",
    eval("Scala is good".toUpperCase())               -> "SCALA IS GOOD",
    eval("SCALA IS GOOD".toLowerCase())               -> "scala is good",
    eval("myemail@gmail.com".matches(".*@gmail.com")) -> true,

    // boolean expressions
    eval(wrap(true) && false) -> false,
    eval(wrap(true) || false) -> true,
    eval(!wrap(true))         -> false,
    eval(!wrap(false))        -> true,
    eval(wrap(true) == false) -> false,

    // union type
    eval("A": Int | String) -> "A",
    eval(1: Int | String)   -> 1,

    // set expressions
    eval(Set(1, 2, 3))                        -> Set(1, 2, 3),
    eval(Set(1, 2, 3) + 4)                    -> Set(1, 2, 3, 4),
    eval(Set(1, 2, 3) - 2)                    -> Set(1, 3),
    eval(Set(1, 2, 3).contains(2))            -> true,
    eval(Set(1, 2, 3).contains(5))            -> false,
    eval(Set(1, 2, 3)(5))                     -> false,
    eval(Set(1, 2, 3) intersect Set(2, 3, 4)) -> Set(2, 3),
    eval(Set(1, 2, 3) ++ Set(2, 3, 4))        -> Set(1, 2, 3, 4),
    eval(Set(1, 2, 3) -- Set(2, 3, 4))        -> Set(1),
    eval(Set(1, 2, 3).filter(_ > 1))          -> Set(2, 3),
    eval(Set(1, 2, 3).map(_ * 2))             -> Set(2, 4, 6),

    // iterable operations
    // iterable.++(iterable)
    eval(List(1, 2, 3) ++ (List(4, 5, 6)))     -> List(1, 2, 3, 4, 5, 6),
    eval(Set(1, 2, 3) ++ (Set(4, 5, 6)))       -> Set(1, 2, 3, 4, 5, 6),
    eval(Vector(1, 2, 3) ++ (Vector(4, 5, 6))) -> Vector(1, 2, 3, 4, 5, 6),
    // // iterable.collect { case a => a }
    // // iterable.collectFirst { case a => a }
    // iterable.concat(iterable)
    eval(List(1, 2, 3).concat(List(4, 5, 6)))     -> List(1, 2, 3, 4, 5, 6),
    eval(Vector(1, 2, 3).concat(Vector(4, 5, 6))) -> Vector(1, 2, 3, 4, 5, 6),
    // TODO: Implement concat for Set
    // eval(Set(1, 2, 3).concat(Set(4, 5, 6)))       -> Set(1, 2, 3, 4, 5, 6),
    // iterable.count(_ => true)
    eval(List(1, 2, 3).count(_ % 2 == 0))   -> 1,
    eval(Set(1, 2, 3).count(_ % 2 == 0))    -> 1,
    eval(Vector(1, 2, 3).count(_ % 2 == 0)) -> 1,
    // iterable.drop(1)
    eval(List(1, 2, 3).drop(1))   -> List(2, 3),
    eval(Set(1, 2, 3).drop(1))    -> Set(2, 3),
    eval(Vector(1, 2, 3).drop(1)) -> Vector(2, 3),
    // iterable.dropRight(1)
    eval(List(1, 2, 3).dropRight(1))   -> List(1, 2),
    eval(Set(1, 2, 3).dropRight(1))    -> Set(1, 2),
    eval(Vector(1, 2, 3).dropRight(1)) -> Vector(1, 2),
    // iterable.dropWhile(_ => true)
    eval(List(1, 2, 3).dropWhile(_ < 2))   -> List(2, 3),
    eval(Set(1, 2, 3).dropWhile(_ < 2))    -> Set(2, 3),
    eval(Vector(1, 2, 3).dropWhile(_ < 2)) -> Vector(2, 3),
    // iterable.empty
    eval(List.empty)   -> List(),
    eval(Nil)          -> List(),
    eval(None)         -> None,
    eval(Set.empty)    -> Set(),
    eval(Vector.empty) -> Vector(),
    eval(Map.empty)    -> Map(),
    // iterable.exists(_ => true)
    eval(List(1, 2, 3).exists(_ > 1))   -> true,
    eval(Set(1, 2, 3).exists(_ > 1))    -> true,
    eval(Vector(1, 2, 3).exists(_ > 1)) -> true,
    // iterable.filter(_ => true)
    eval(List(1, 2, 3).filter(_ > 1))   -> List(2, 3),
    eval(Set(1, 2, 3).filter(_ > 1))    -> Set(2, 3),
    eval(Vector(1, 2, 3).filter(_ > 1)) -> Vector(2, 3),
    // iterable.filterNot(_ => true)
    eval(List(1, 2, 3).filterNot(_ > 1)) -> List(1),
    eval(Set(1, 2, 3).filterNot(_ > 1))  -> Set(1),
    // eval(Vector(1, 2, 3).filterNot(_ > 1)) -> Vector(1),
    // iterable.find(_ => true)
    eval(List(1, 2, 3).find(_ > 1))   -> Some(2),
    eval(Set(1, 2, 3).find(_ > 1))    -> Some(2),
    eval(Vector(1, 2, 3).find(_ > 1)) -> Some(2),
    // iterable.flatMap(a => List(a))
    eval(List(1, 2, 3).flatMap(a => List(a, a * 2)))     -> List(1, 2, 2, 4, 3, 6),
    eval(Set(1, 2, 3).flatMap(a => Set(a, a * 2)))       -> Set(1, 2, 3, 4, 6),
    eval(Vector(1, 2, 3).flatMap(a => Vector(a, a * 2))) -> Vector(1, 2, 2, 4, 3, 6),
    // iterable.flatten
    // eval(List("hello".toUpperCase(), "nice") -> List("HELLO"),
    eval(List(List(1), List(2), List(3)).flatten)         -> List(1, 2, 3),
    eval(Set(Set(1), Set(2), Set(3)).flatten)             -> Set(1, 2, 3),
    eval(Vector(Vector(1), Vector(2), Vector(3)).flatten) -> Vector(1, 2, 3),
    // iterable.fold(0)(_ + _)
    eval(List(1, 2, 3).fold(0)(_ + _))   -> 6,
    eval(Set(1, 2, 3).fold(0)(_ + _))    -> 6,
    eval(Vector(1, 2, 3).fold(0)(_ + _)) -> 6,
    // iterable.foldLeft(0)(_ + _)
    eval(List(1, 2, 3).foldLeft(0)(_ + _))   -> 6,
    eval(Set(1, 2, 3).foldLeft(0)(_ + _))    -> 6,
    eval(Vector(1, 2, 3).foldLeft(0)(_ + _)) -> 6,
    // iterable.foldRight(0)(_ + _)
    eval(List(1, 2, 3).foldRight(0)(_ + _))   -> 6,
    eval(Set(1, 2, 3).foldRight(0)(_ + _))    -> 6,
    eval(Vector(1, 2, 3).foldRight(0)(_ + _)) -> 6,
    // iterable.forall(_ => true)
    eval(List(1, 2, 3).forall(_ => true))   -> true,
    eval(Set(1, 2, 3).forall(_ => true))    -> true,
    eval(Vector(1, 2, 3).forall(_ => true)) -> true,
    // iterable.foreach(_ => ())
    eval(List(1, 2, 3).foreach(_ => ()))   -> (),
    eval(Set(1, 2, 3).foreach(_ => ()))    -> (),
    eval(Vector(1, 2, 3).foreach(_ => ())) -> (),
    // iterable.groupBy(_ => 0)
    eval(List(1, 2, 3).groupBy(_ => 0))   -> Map(0 -> List(1, 2, 3)),
    eval(Set(1, 2, 3).groupBy(_ => 0))    -> Map(0 -> Set(1, 2, 3)),
    eval(Vector(1, 2, 3).groupBy(_ => 0)) -> Map(0 -> Vector(1, 2, 3)),
    // iterable.groupMap(_ => 0)(_ => 0)
    eval(List(1, 2, 3).groupMap(_ => 0)(_ => 0))   -> Map(0 -> List(0, 0, 0)),
    eval(Set(1, 2, 3).groupMap(_ => 0)(_ => 0))    -> Map(0 -> Set(0)),
    eval(Vector(1, 2, 3).groupMap(_ => 0)(_ => 0)) -> Map(0 -> Vector(0, 0, 0)),
    // iterable.groupMapReduce(_ => 0)(_ => 0)(_ + _)
    eval(List(1, 2, 3).groupMapReduce(_ => 0)(_ => 0)(_ + _))   -> Map(0 -> 0),
    eval(Set(1, 2, 3).groupMapReduce(_ => 0)(_ => 0)(_ + _))    -> Map(0 -> 0),
    eval(Vector(1, 2, 3).groupMapReduce(_ => 0)(_ => 0)(_ + _)) -> Map(0 -> 0),
    // iterable.grouped(1)
    // iterable.head
    // iterable.headOption
    // iterable.init
    // iterable.inits
    // iterable.knownSize
    // iterable.isEmpty
    // iterable.last
    // iterable.lastOption
    // iterable.map(a => a)
    // iterable.max
    eval(List(1, 2, 3).max)   -> 3,
    eval(Set(1, 2, 3).max)    -> 3,
    eval(Vector(1, 2, 3).max) -> 3,
    // iterable.maxOption
    // iterable.maxBy(_ => 0)
    // iterable.maxByOption(_ => 0)
    // iterable.mkString
    // iterable.mkString(",")
    // iterable.mkString("[", ",", "]")
    // iterable.min
    // iterable.minOption
    // iterable.minBy(_ => 0)
    // iterable.minByOption(_ => 0)
    // iterable.nonEmpty
    // iterable.partition(_ => true)
    // iterable.partitionMap(a => Right(a))
    // iterable.reduce(_ + _)
    // iterable.reduceOption(_ + _)
    // iterable.reduceLeft(_ + _)
    // iterable.reduceLeftOption(_ + _)
    // iterable.reduceRight(_ + _)
    // iterable.reduceRightOption(_ + _)
    // iterable.scan(0)(_ + _)
    // iterable.scanLeft(0)(_ + _)
    // iterable.scanRight(0)(_ + _)
    // iterable.size
    // iterable.sizeCompare(0)
    // iterable.sizeCompare(iterable)
    // iterable.sizeIs > 5
    // iterable.slice(0, 1)
    // iterable.sliding(3)
    // iterable.sliding(3, 1)
    // iterable.span(_ => true)
    // iterable.splitAt(1)
    // iterable.stepper
    // iterable.sum
    // iterable.tail
    // iterable.tails
    // iterable.take(1)
    // iterable.takeRight(1)
    // iterable.takeWhile(_ => true)
    // iterable.tapEach(_ => ())
    // iterable.toArray
    // iterable.toBuffer
    // iterable.toIndexedSeq
    // iterable.toList
    // // iterable.toMap
    // iterable.toSeq
    // iterable.toSet
    // iterable.toVector
    // // iterable.transpose
    // // iterable.unzip
    // // iterable.unzip3
    // iterable.view
    // iterable.withFilter(_ => true)
    // iterable.zip(iterable)
    // iterable.zipAll(iterable, 0, 0)
    // iterable.zipWithIndex

    // list expressions
    eval(List(1, 2, 3)(2))   -> 3,
    eval(List(1, 2, 3))      -> List(1, 2, 3),
    eval(List(1, 2, 3) :+ 4) -> List(1, 2, 3, 4),
    eval(5 :: List(1, 2, 3)) -> List(5, 1, 2, 3),
    eval(List(1, 2, 3).head) -> 1,
    // filter, map, flatMap
    eval(List(1, 2, 3).filter(_ > 1)) -> List(2, 3),
    eval(List(1, 2, 3).map(_ * 2))    -> List(2, 4, 6),
    // eval(List(1, 2, 3).flatMap(a => List(a, a))) -> List(1, 1, 2, 2, 3, 3),
    // isEmpty, nonEmpty
    eval(List(1, 2, 3).isEmpty)  -> false,
    eval(List(1, 2, 3).nonEmpty) -> true,

    // isInstanceOf + asInstanceOf
    eval(List(1).isInstanceOf[List[Int]]) -> true,
    eval(List(1).asInstanceOf[List[Int]]) -> List(1),
    eval {
      val x = 10
      if x > 5 then x else "hello"
    } -> 10,
    // TODO: lambdas
    eval {
      def f(x: Int) = x + 1
      f(10)
    } -> 11,
    eval {
      List(0, 1, 2, 3).map(_ * 2)
    }                                         -> List(0, 2, 4, 6),
    eval(List(0, 1, 2, 3).filter(a => a > 1)) -> List(2, 3),
    eval {
      val x = Person("Hello", 12)
      x.name
    } -> "Hello"
  )

case class Person(name: String, age: Int)

object IterableTest:
  eval {
    val iterable = List(1, 2, 3)
    // iterable.
    // iterable.++(iterable)
    // // iterable.collect { case a => a }
    // // iterable.collectFirst { case a => a }
    // iterable.concat(iterable)
    // iterable.count(_ => true)
    // iterable.drop(1)
    // iterable.dropRight(1)
    // iterable.dropWhile(_ => true)
    // iterable.empty
    // iterable.exists(_ => true)
    // iterable.filter(_ => true)
    // iterable.filterNot(_ => true)
    // iterable.find(_ => true)
    // iterable.flatMap(a => List(a))
    // // iterable.flatten
    // iterable.fold(0)(_ + _)
    // iterable.foldLeft(0)(_ + _)
    // iterable.foldRight(0)(_ + _)
    // iterable.forall(_ => true)
    // iterable.foreach(_ => ())
    // iterable.groupBy(_ => 0)
    // iterable.groupMap(_ => 0)(_ => 0)
    iterable.groupMapReduce(_ => 0)(_ => 0)(_ + _)
    // iterable.grouped(1)
    // iterable.head
    // iterable.headOption
    // iterable.init
    // iterable.inits
    // iterable.knownSize
    // iterable.isEmpty
    // iterable.last
    // iterable.lastOption
    // iterable.map(a => a)
    // iterable.max
    // iterable.maxOption
    // iterable.maxBy(_ => 0)
    // iterable.maxByOption(_ => 0)
    // iterable.mkString
    // iterable.mkString(",")
    // iterable.mkString("[", ",", "]")
    // iterable.min
    // iterable.minOption
    // iterable.minBy(_ => 0)
    // iterable.minByOption(_ => 0)
    // iterable.nonEmpty
    // iterable.partition(_ => true)
    // iterable.partitionMap(a => Right(a))
    // iterable.reduce(_ + _)
    // iterable.reduceOption(_ + _)
    // iterable.reduceLeft(_ + _)
    // iterable.reduceLeftOption(_ + _)
    // iterable.reduceRight(_ + _)
    // iterable.reduceRightOption(_ + _)
    // iterable.scan(0)(_ + _)
    // iterable.scanLeft(0)(_ + _)
    // iterable.scanRight(0)(_ + _)
    // iterable.size
    // iterable.sizeCompare(0)
    // iterable.sizeCompare(iterable)
    // iterable.sizeIs > 5
    // iterable.slice(0, 1)
    // iterable.sliding(3)
    // iterable.sliding(3, 1)
    // iterable.span(_ => true)
    // iterable.splitAt(1)
    // iterable.stepper
    // iterable.sum
    // iterable.tail
    // iterable.tails
    // iterable.take(1)
    // iterable.takeRight(1)
    // iterable.takeWhile(_ => true)
    // iterable.tapEach(_ => ())
    // iterable.toArray
    // iterable.toBuffer
    // iterable.toIndexedSeq
    // iterable.toList
    // // iterable.toMap
    // iterable.toSeq
    // iterable.toSet
    // iterable.toVector
    // // iterable.transpose
    // // iterable.unzip
    // // iterable.unzip3
    // iterable.view
    // iterable.withFilter(_ => true)
    // iterable.zip(iterable)
    // iterable.zipAll(iterable, 0, 0)
    // iterable.zipWithIndex

    0 // TODO: Extend FromExpr to work with Some(1), (), etc.

    // Case Classes

  }
