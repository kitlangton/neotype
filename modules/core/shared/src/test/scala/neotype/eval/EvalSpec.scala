package neotype.eval

import neotype.TestMacros.*
import zio.test.*

import scala.util.Try

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
    eval(List(1, 2, 3).filterNot(_ > 1))   -> List(1),
    eval(Set(1, 2, 3).filterNot(_ > 1))    -> Set(1),
    eval(Vector(1, 2, 3).filterNot(_ > 1)) -> Vector(1),
    // iterable.find(_ => true)
    eval(List(1, 2, 3).find(_ > 1))   -> Some(2),
    eval(Set(1, 2, 3).find(_ > 1))    -> Some(2),
    eval(Vector(1, 2, 3).find(_ > 1)) -> Some(2),
    // iterable.flatMap(a => List(a))
    eval(List(1, 2, 3).flatMap(a => List(a, a * 2)))     -> List(1, 2, 2, 4, 3, 6),
    eval(Set(1, 2, 3).flatMap(a => Set(a, a * 2)))       -> Set(1, 2, 3, 4, 6),
    eval(Vector(1, 2, 3).flatMap(a => Vector(a, a * 2))) -> Vector(1, 2, 2, 4, 3, 6),
    // iterable.flatten
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
    eval(List(1, 2, 3).grouped(1).toList)     -> List(List(1), List(2), List(3)),
    eval(Set(1, 2, 3).grouped(1).toSet)       -> Set(Set(1), Set(2), Set(3)),
    eval(Vector(1, 2, 3).grouped(1).toVector) -> Vector(Vector(1), Vector(2), Vector(3)),
    // iterable.head
    eval(List(1, 2, 3).head)   -> 1,
    eval(Set(1, 2, 3).head)    -> 1,
    eval(Vector(1, 2, 3).head) -> 1,
    // iterable.headOption
    eval(List(1, 2, 3).headOption)   -> Some(1),
    eval(Set(1, 2, 3).headOption)    -> Some(1),
    eval(Vector(1, 2, 3).headOption) -> Some(1),
    // iterable.init
    eval(List(1, 2, 3).init)   -> List(1, 2),
    eval(Vector(1, 2, 3).init) -> Vector(1, 2),
    eval(Set(1, 2, 3).init)    -> Set(1, 2),
    // iterable.inits
    eval(List(1, 2, 3).inits.toList)   -> List(List(1, 2, 3), List(1, 2), List(1), List()),
    eval(Vector(1, 2, 3).inits.toList) -> List(Vector(1, 2, 3), Vector(1, 2), Vector(1), Vector()),
    eval(Set(1, 2, 3).inits.toList)    -> List(Set(1, 2, 3), Set(1, 2), Set(1), Set()),
    // iterable.knownSize
    eval(List(1, 2, 3).knownSize)   -> -1, // "if it can be cheaply computed, -1 otherwise"
    eval(Vector(1, 2, 3).knownSize) -> 3,
    eval(Set(1, 2, 3).knownSize)    -> 3,
    // iterable.isEmpty
    eval(List(1, 2, 3).isEmpty)   -> false,
    eval(Vector(1, 2, 3).isEmpty) -> false,
    eval(Set(1, 2, 3).isEmpty)    -> false,
    eval(List().isEmpty)          -> true,
    eval(Vector().isEmpty)        -> true,
    eval(Set().isEmpty)           -> true,

    // iterable.last
    eval(List(1, 2, 3).last)   -> 3,
    eval(Vector(1, 2, 3).last) -> 3,
    eval(Set(1, 2, 3).last)    -> 3,

    // iterable.lastOption
    eval(List(1, 2, 3).lastOption)   -> Some(3),
    eval(Vector(1, 2, 3).lastOption) -> Some(3),
    eval(Set(1, 2, 3).lastOption)    -> Some(3),
    eval(List().lastOption)          -> None,
    eval(Vector().lastOption)        -> None,
    eval(Set().lastOption)           -> None,

    // iterable.map(a => a)
    eval(List(1, 2, 3).map(a => a))   -> List(1, 2, 3),
    eval(Vector(1, 2, 3).map(a => a)) -> Vector(1, 2, 3),
    eval(Set(1, 2, 3).map(a => a))    -> Set(1, 2, 3),
    // iterable.max
    eval(List(1, 2, 3).max)   -> 3,
    eval(Set(1, 2, 3).max)    -> 3,
    eval(Vector(1, 2, 3).max) -> 3,
    // iterable.maxOption
    eval(List(1, 2, 3).maxOption)     -> Some(3),
    eval(Vector(1, 2, 3).maxOption)   -> Some(3),
    eval(Set(1, 2, 3).maxOption)      -> Some(3),
    eval(List.empty[Int].maxOption)   -> None,
    eval(Vector.empty[Int].maxOption) -> None,
    eval(Set.empty[Int].maxOption)    -> None,

    // iterable.maxBy(_ => 0)
    eval(List(1, 2, 3).maxBy(_ => 0))   -> 1,
    eval(Vector(1, 2, 3).maxBy(_ => 0)) -> 1,
    eval(Set(1, 2, 3).maxBy(_ => 0))    -> 1,

    // iterable.maxByOption(_ => 0)
    eval(List(1, 2, 3).maxByOption(_ => 0))     -> Some(1),
    eval(Vector(1, 2, 3).maxByOption(_ => 0))   -> Some(1),
    eval(Set(1, 2, 3).maxByOption(_ => 0))      -> Some(1),
    eval(List.empty[Int].maxByOption(_ => 0))   -> None,
    eval(Vector.empty[Int].maxByOption(_ => 0)) -> None,
    eval(Set.empty[Int].maxByOption(_ => 0))    -> None,

    // iterable.mkString
    eval(List(1, 2, 3).mkString)   -> "123",
    eval(Vector(1, 2, 3).mkString) -> "123",
    eval(Set(1, 2, 3).mkString)    -> "123",

    // iterable.mkString(",")
    eval(List(1, 2, 3).mkString(","))   -> "1,2,3",
    eval(Vector(1, 2, 3).mkString(",")) -> "1,2,3",
    eval(Set(1, 2, 3).mkString(","))    -> "1,2,3",

    // iterable.mkString("[", ",", "]")
    eval(List(1, 2, 3).mkString("[", ",", "]"))   -> "[1,2,3]",
    eval(Vector(1, 2, 3).mkString("[", ",", "]")) -> "[1,2,3]",
    eval(Set(1, 2, 3).mkString("[", ",", "]"))    -> "[1,2,3]",

    // iterable.min
    eval(List(1, 2, 3).min)   -> 1,
    eval(Vector(1, 2, 3).min) -> 1,
    eval(Set(1, 2, 3).min)    -> 1,

    // iterable.minOption
    eval(List(1, 2, 3).minOption)     -> Some(1),
    eval(Vector(1, 2, 3).minOption)   -> Some(1),
    eval(Set(1, 2, 3).minOption)      -> Some(1),
    eval(List.empty[Int].minOption)   -> None,
    eval(Vector.empty[Int].minOption) -> None,
    eval(Set.empty[Int].minOption)    -> None,
    // iterable.minBy(_ => 0)
    eval(List(1, 2, 3).minBy(_ => 0))   -> 1,
    eval(Vector(1, 2, 3).minBy(_ => 0)) -> 1,
    eval(Set(1, 2, 3).minBy(_ => 0))    -> 1,

    // iterable.minByOption(_ => 0)
    eval(List(1, 2, 3).minByOption(_ => 0))     -> Some(1),
    eval(Vector(1, 2, 3).minByOption(_ => 0))   -> Some(1),
    eval(Set(1, 2, 3).minByOption(_ => 0))      -> Some(1),
    eval(List.empty[Int].minByOption(_ => 0))   -> None,
    eval(Vector.empty[Int].minByOption(_ => 0)) -> None,
    eval(Set.empty[Int].minByOption(_ => 0))    -> None,

    // iterable.nonEmpty
    eval(List(1, 2, 3).nonEmpty)     -> true,
    eval(Vector(1, 2, 3).nonEmpty)   -> true,
    eval(Set(1, 2, 3).nonEmpty)      -> true,
    eval(List.empty[Int].nonEmpty)   -> false,
    eval(Vector.empty[Int].nonEmpty) -> false,
    eval(Set.empty[Int].nonEmpty)    -> false,

    // iterable.partition(_ => true)
    eval(List(1, 2, 3).partition(_ => true))   -> (List(1, 2, 3), List()),
    eval(Vector(1, 2, 3).partition(_ => true)) -> (Vector(1, 2, 3), Vector()),
    eval(Set(1, 2, 3).partition(_ => true))    -> (Set(1, 2, 3), Set()),

    // iterable.partitionMap(a => Right(a))
    eval(List(1, 2, 3).partitionMap(a => Right(a)))   -> (List(), List(1, 2, 3)),
    eval(Vector(1, 2, 3).partitionMap(a => Right(a))) -> (Vector(), Vector(1, 2, 3)),
    eval(Set(1, 2, 3).partitionMap(a => Right(a)))    -> (Set(), Set(1, 2, 3)),

    // iterable.reduce(_ + _)
    eval(List(1, 2, 3).reduce(_ + _))   -> 6,
    eval(Vector(1, 2, 3).reduce(_ + _)) -> 6,
    eval(Set(1, 2, 3).reduce(_ + _))    -> 6,

    // iterable.reduceOption(_ + _)
    eval(List(1, 2, 3).reduceOption(_ + _))     -> Some(6),
    eval(Vector(1, 2, 3).reduceOption(_ + _))   -> Some(6),
    eval(Set(1, 2, 3).reduceOption(_ + _))      -> Some(6),
    eval(List.empty[Int].reduceOption(_ + _))   -> None,
    eval(Vector.empty[Int].reduceOption(_ + _)) -> None,
    eval(Set.empty[Int].reduceOption(_ + _))    -> None,

    // iterable.reduceLeft(_ + _)
    eval(List(1, 2, 3).reduceLeft(_ + _))   -> 6,
    eval(Vector(1, 2, 3).reduceLeft(_ + _)) -> 6,
    eval(Set(1, 2, 3).reduceLeft(_ + _))    -> 6,

    // iterable.reduceLeftOption(_ + _)
    eval(List(1, 2, 3).reduceLeftOption(_ + _))     -> Some(6),
    eval(Vector(1, 2, 3).reduceLeftOption(_ + _))   -> Some(6),
    eval(Set(1, 2, 3).reduceLeftOption(_ + _))      -> Some(6),
    eval(List.empty[Int].reduceLeftOption(_ + _))   -> None,
    eval(Vector.empty[Int].reduceLeftOption(_ + _)) -> None,
    eval(Set.empty[Int].reduceLeftOption(_ + _))    -> None,

    // iterable.reduceRight(_ + _)
    eval(List(1, 2, 3).reduceRight(_ + _))   -> 6,
    eval(Vector(1, 2, 3).reduceRight(_ + _)) -> 6,
    eval(Set(1, 2, 3).reduceRight(_ + _))    -> 6,

    // iterable.reduceRightOption(_ + _)
    eval(List(1, 2, 3).reduceRightOption(_ + _))     -> Some(6),
    eval(Vector(1, 2, 3).reduceRightOption(_ + _))   -> Some(6),
    eval(Set(1, 2, 3).reduceRightOption(_ + _))      -> Some(6),
    eval(List.empty[Int].reduceRightOption(_ + _))   -> None,
    eval(Vector.empty[Int].reduceRightOption(_ + _)) -> None,
    eval(Set.empty[Int].reduceRightOption(_ + _))    -> None,
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
    // iterable.transpose
    eval(List(List(1, 2, 3), List(4, 5, 6)).transpose) -> List(List(1, 4), List(2, 5), List(3, 6)),
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

    // BigDecimal
    eval(BigDecimal("123.456")) -> BigDecimal(123.456),
    eval(BigDecimal(123.456))   -> BigDecimal(123.456),
    eval(BigDecimal(123))       -> BigDecimal(123),
    eval(BigDecimal(123L))      -> BigDecimal(123),
    eval(BigDecimal(123.456d))  -> BigDecimal(123.456),

    // Try
    eval(Try(1 / 1))                 -> scala.util.Success(1),
    eval(Try(BigDecimal("123.456"))) -> scala.util.Success(BigDecimal(123.456)),

    // isInstanceOf + asInstanceOf
    eval(List(1).isInstanceOf[List[Int]]) -> true,
    eval(List(1).asInstanceOf[List[Int]]) -> List(1),
    eval {
      val x = 10
      if x > 5 then x else "hello"
    } -> 10,

    // lambdas
    eval {
      def f(x: Int) = x + 1
      f(10)
    } -> 11,
    eval {
      List(0, 1, 2, 3).map(_ * 2)
    }                                         -> List(0, 2, 4, 6),
    eval(List(0, 1, 2, 3).filter(a => a > 1)) -> List(2, 3),
    eval {
      val x = Person("Hello", 3)
      x.name * x.age
    } -> "HelloHelloHello"
  )

case class Person(name: String, age: Int)
