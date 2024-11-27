package neotype.eval

import neotype.TestMacros.*
import zio.test.*

object IterableSpec extends ZIOSpecDefault:
  val spec =
    suite("IterableSpec")(
      evalTests.map { case (actual, expected) =>
        test(s"eval($actual) == $expected") {
          assertTrue(actual == expected)
        }
      }
    )

  inline def wrap[A](a: A): A = a

  lazy val evalTests = List(
    // Concatenation operations
    eval(List(1, 2, 3) ++ (List(4, 5, 6)))        -> List(1, 2, 3, 4, 5, 6),
    eval(Set(1, 2, 3) ++ (Set(4, 5, 6)))          -> Set(1, 2, 3, 4, 5, 6),
    eval(Vector(1, 2, 3) ++ (Vector(4, 5, 6)))    -> Vector(1, 2, 3, 4, 5, 6),
    eval(List(1, 2, 3).concat(List(4, 5, 6)))     -> List(1, 2, 3, 4, 5, 6),
    eval(Vector(1, 2, 3).concat(Vector(4, 5, 6))) -> Vector(1, 2, 3, 4, 5, 6),

    // Element operations
    eval(List(1, 2, 3).count(_ % 2 == 0))   -> 1,
    eval(Set(1, 2, 3).count(_ % 2 == 0))    -> 1,
    eval(Vector(1, 2, 3).count(_ % 2 == 0)) -> 1,

    // Drop operations
    eval(List(1, 2, 3).drop(1))            -> List(2, 3),
    eval(Set(1, 2, 3).drop(1))             -> Set(2, 3),
    eval(Vector(1, 2, 3).drop(1))          -> Vector(2, 3),
    eval(List(1, 2, 3).dropRight(1))       -> List(1, 2),
    eval(Set(1, 2, 3).dropRight(1))        -> Set(1, 2),
    eval(Vector(1, 2, 3).dropRight(1))     -> Vector(1, 2),
    eval(List(1, 2, 3).dropWhile(_ < 2))   -> List(2, 3),
    eval(Set(1, 2, 3).dropWhile(_ < 2))    -> Set(2, 3),
    eval(Vector(1, 2, 3).dropWhile(_ < 2)) -> Vector(2, 3),

    // Empty collections
    eval(List.empty)   -> List(),
    eval(Nil)          -> List(),
    eval(None)         -> None,
    eval(Set.empty)    -> Set(),
    eval(Vector.empty) -> Vector(),
    eval(Map.empty)    -> Map(),

    // Existence checks
    eval(List(1, 2, 3).exists(_ > 1))   -> true,
    eval(Set(1, 2, 3).exists(_ > 1))    -> true,
    eval(Vector(1, 2, 3).exists(_ > 1)) -> true,

    // Filter operations
    eval(List(1, 2, 3).filter(_ > 1))      -> List(2, 3),
    eval(Set(1, 2, 3).filter(_ > 1))       -> Set(2, 3),
    eval(Vector(1, 2, 3).filter(_ > 1))    -> Vector(2, 3),
    eval(List(1, 2, 3).filterNot(_ > 1))   -> List(1),
    eval(Set(1, 2, 3).filterNot(_ > 1))    -> Set(1),
    eval(Vector(1, 2, 3).filterNot(_ > 1)) -> Vector(1),

    // Find operations
    eval(List(1, 2, 3).find(_ > 1))   -> Some(2),
    eval(Set(1, 2, 3).find(_ > 1))    -> Some(2),
    eval(Vector(1, 2, 3).find(_ > 1)) -> Some(2),

    // FlatMap and flatten
    eval(List(1, 2, 3).flatMap(a => List(a, a * 2)))      -> List(1, 2, 2, 4, 3, 6),
    eval(Set(1, 2, 3).flatMap(a => Set(a, a * 2)))        -> Set(1, 2, 3, 4, 6),
    eval(Vector(1, 2, 3).flatMap(a => Vector(a, a * 2)))  -> Vector(1, 2, 2, 4, 3, 6),
    eval(List(List(1), List(2), List(3)).flatten)         -> List(1, 2, 3),
    eval(Set(Set(1), Set(2), Set(3)).flatten)             -> Set(1, 2, 3),
    eval(Vector(Vector(1), Vector(2), Vector(3)).flatten) -> Vector(1, 2, 3),

    // Fold operations
    eval(List(1, 2, 3).fold(0)(_ + _))        -> 6,
    eval(Set(1, 2, 3).fold(0)(_ + _))         -> 6,
    eval(Vector(1, 2, 3).fold(0)(_ + _))      -> 6,
    eval(List(1, 2, 3).foldLeft(0)(_ + _))    -> 6,
    eval(Set(1, 2, 3).foldLeft(0)(_ + _))     -> 6,
    eval(Vector(1, 2, 3).foldLeft(0)(_ + _))  -> 6,
    eval(List(1, 2, 3).foldRight(0)(_ + _))   -> 6,
    eval(Set(1, 2, 3).foldRight(0)(_ + _))    -> 6,
    eval(Vector(1, 2, 3).foldRight(0)(_ + _)) -> 6,

    // ForAll operations
    eval(List(1, 2, 3).forall(_ => true))   -> true,
    eval(Set(1, 2, 3).forall(_ => true))    -> true,
    eval(Vector(1, 2, 3).forall(_ => true)) -> true,

    // Foreach operations
    eval(List(1, 2, 3).foreach(_ => ()))   -> (),
    eval(Set(1, 2, 3).foreach(_ => ()))    -> (),
    eval(Vector(1, 2, 3).foreach(_ => ())) -> (),

    // Grouping operations
    eval(List(1, 2, 3).groupBy(_ => 0))                         -> Map(0 -> List(1, 2, 3)),
    eval(Set(1, 2, 3).groupBy(_ => 0))                          -> Map(0 -> Set(1, 2, 3)),
    eval(Vector(1, 2, 3).groupBy(_ => 0))                       -> Map(0 -> Vector(1, 2, 3)),
    eval(List(1, 2, 3).groupMap(_ => 0)(_ => 0))                -> Map(0 -> List(0, 0, 0)),
    eval(Set(1, 2, 3).groupMap(_ => 0)(_ => 0))                 -> Map(0 -> Set(0)),
    eval(Vector(1, 2, 3).groupMap(_ => 0)(_ => 0))              -> Map(0 -> Vector(0, 0, 0)),
    eval(List(1, 2, 3).groupMapReduce(_ => 0)(_ => 0)(_ + _))   -> Map(0 -> 0),
    eval(Set(1, 2, 3).groupMapReduce(_ => 0)(_ => 0)(_ + _))    -> Map(0 -> 0),
    eval(Vector(1, 2, 3).groupMapReduce(_ => 0)(_ => 0)(_ + _)) -> Map(0 -> 0),

    // Grouped operations
    eval(List(1, 2, 3).grouped(1).toList)     -> List(List(1), List(2), List(3)),
    eval(Set(1, 2, 3).grouped(1).toSet)       -> Set(Set(1), Set(2), Set(3)),
    eval(Vector(1, 2, 3).grouped(1).toVector) -> Vector(Vector(1), Vector(2), Vector(3)),

    // Head operations
    eval(List(1, 2, 3).head)         -> 1,
    eval(Set(1, 2, 3).head)          -> 1,
    eval(Vector(1, 2, 3).head)       -> 1,
    eval(List(1, 2, 3).headOption)   -> Some(1),
    eval(Set(1, 2, 3).headOption)    -> Some(1),
    eval(Vector(1, 2, 3).headOption) -> Some(1),

    // Init operations
    eval(List(1, 2, 3).init)           -> List(1, 2),
    eval(Vector(1, 2, 3).init)         -> Vector(1, 2),
    eval(Set(1, 2, 3).init)            -> Set(1, 2),
    eval(List(1, 2, 3).inits.toList)   -> List(List(1, 2, 3), List(1, 2), List(1), List()),
    eval(Vector(1, 2, 3).inits.toList) -> List(Vector(1, 2, 3), Vector(1, 2), Vector(1), Vector()),
    eval(Set(1, 2, 3).inits.toList)    -> List(Set(1, 2, 3), Set(1, 2), Set(1), Set()),

    // Size operations
    eval(List(1, 2, 3).knownSize)   -> -1,
    eval(Vector(1, 2, 3).knownSize) -> 3,
    eval(Set(1, 2, 3).knownSize)    -> 3,

    // Empty checks
    eval(List(1, 2, 3).isEmpty)   -> false,
    eval(Vector(1, 2, 3).isEmpty) -> false,
    eval(Set(1, 2, 3).isEmpty)    -> false,
    eval(List().isEmpty)          -> true,
    eval(Vector().isEmpty)        -> true,
    eval(Set().isEmpty)           -> true,

    // Last operations
    eval(List(1, 2, 3).last)         -> 3,
    eval(Vector(1, 2, 3).last)       -> 3,
    eval(Set(1, 2, 3).last)          -> 3,
    eval(List(1, 2, 3).lastOption)   -> Some(3),
    eval(Vector(1, 2, 3).lastOption) -> Some(3),
    eval(Set(1, 2, 3).lastOption)    -> Some(3),
    eval(List().lastOption)          -> None,
    eval(Vector().lastOption)        -> None,
    eval(Set().lastOption)           -> None,

    // Map operations
    eval(List(1, 2, 3).map(a => a))   -> List(1, 2, 3),
    eval(Vector(1, 2, 3).map(a => a)) -> Vector(1, 2, 3),
    eval(Set(1, 2, 3).map(a => a))    -> Set(1, 2, 3),

    // Max/Min operations
    eval(List(1, 2, 3).max)                     -> 3,
    eval(Set(1, 2, 3).max)                      -> 3,
    eval(Vector(1, 2, 3).max)                   -> 3,
    eval(List(1, 2, 3).maxOption)               -> Some(3),
    eval(Vector(1, 2, 3).maxOption)             -> Some(3),
    eval(Set(1, 2, 3).maxOption)                -> Some(3),
    eval(List.empty[Int].maxOption)             -> None,
    eval(Vector.empty[Int].maxOption)           -> None,
    eval(Set.empty[Int].maxOption)              -> None,
    eval(List(1, 2, 3).maxBy(_ => 0))           -> 1,
    eval(Vector(1, 2, 3).maxBy(_ => 0))         -> 1,
    eval(Set(1, 2, 3).maxBy(_ => 0))            -> 1,
    eval(List(1, 2, 3).maxByOption(_ => 0))     -> Some(1),
    eval(Vector(1, 2, 3).maxByOption(_ => 0))   -> Some(1),
    eval(Set(1, 2, 3).maxByOption(_ => 0))      -> Some(1),
    eval(List.empty[Int].maxByOption(_ => 0))   -> None,
    eval(Vector.empty[Int].maxByOption(_ => 0)) -> None,
    eval(Set.empty[Int].maxByOption(_ => 0))    -> None,
    eval(List(1, 2, 3).min)                     -> 1,
    eval(Vector(1, 2, 3).min)                   -> 1,
    eval(Set(1, 2, 3).min)                      -> 1,
    eval(List(1, 2, 3).minOption)               -> Some(1),
    eval(Vector(1, 2, 3).minOption)             -> Some(1),
    eval(Set(1, 2, 3).minOption)                -> Some(1),
    eval(List.empty[Int].minOption)             -> None,
    eval(Vector.empty[Int].minOption)           -> None,
    eval(Set.empty[Int].minOption)              -> None,
    eval(List(1, 2, 3).minBy(_ => 0))           -> 1,
    eval(Vector(1, 2, 3).minBy(_ => 0))         -> 1,
    eval(Set(1, 2, 3).minBy(_ => 0))            -> 1,
    eval(List(1, 2, 3).minByOption(_ => 0))     -> Some(1),
    eval(Vector(1, 2, 3).minByOption(_ => 0))   -> Some(1),
    eval(Set(1, 2, 3).minByOption(_ => 0))      -> Some(1),
    eval(List.empty[Int].minByOption(_ => 0))   -> None,
    eval(Vector.empty[Int].minByOption(_ => 0)) -> None,
    eval(Set.empty[Int].minByOption(_ => 0))    -> None,

    // NonEmpty checks
    eval(List(1, 2, 3).nonEmpty)     -> true,
    eval(Vector(1, 2, 3).nonEmpty)   -> true,
    eval(Set(1, 2, 3).nonEmpty)      -> true,
    eval(List.empty[Int].nonEmpty)   -> false,
    eval(Vector.empty[Int].nonEmpty) -> false,
    eval(Set.empty[Int].nonEmpty)    -> false,

    // Partition operations
    eval(List(1, 2, 3).partition(_ => true))          -> (List(1, 2, 3), List()),
    eval(Vector(1, 2, 3).partition(_ => true))        -> (Vector(1, 2, 3), Vector()),
    eval(Set(1, 2, 3).partition(_ => true))           -> (Set(1, 2, 3), Set()),
    eval(List(1, 2, 3).partitionMap(a => Right(a)))   -> (List(), List(1, 2, 3)),
    eval(Vector(1, 2, 3).partitionMap(a => Right(a))) -> (Vector(), Vector(1, 2, 3)),
    eval(Set(1, 2, 3).partitionMap(a => Right(a)))    -> (Set(), Set(1, 2, 3)),

    // Reduce operations
    eval(List(1, 2, 3).reduce(_ + _))                -> 6,
    eval(Vector(1, 2, 3).reduce(_ + _))              -> 6,
    eval(Set(1, 2, 3).reduce(_ + _))                 -> 6,
    eval(List(1, 2, 3).reduceOption(_ + _))          -> Some(6),
    eval(Vector(1, 2, 3).reduceOption(_ + _))        -> Some(6),
    eval(Set(1, 2, 3).reduceOption(_ + _))           -> Some(6),
    eval(List.empty[Int].reduceOption(_ + _))        -> None,
    eval(Vector.empty[Int].reduceOption(_ + _))      -> None,
    eval(Set.empty[Int].reduceOption(_ + _))         -> None,
    eval(List(1, 2, 3).reduceLeft(_ + _))            -> 6,
    eval(Vector(1, 2, 3).reduceLeft(_ + _))          -> 6,
    eval(Set(1, 2, 3).reduceLeft(_ + _))             -> 6,
    eval(List(1, 2, 3).reduceLeftOption(_ + _))      -> Some(6),
    eval(Vector(1, 2, 3).reduceLeftOption(_ + _))    -> Some(6),
    eval(Set(1, 2, 3).reduceLeftOption(_ + _))       -> Some(6),
    eval(List.empty[Int].reduceLeftOption(_ + _))    -> None,
    eval(Vector.empty[Int].reduceLeftOption(_ + _))  -> None,
    eval(Set.empty[Int].reduceLeftOption(_ + _))     -> None,
    eval(List(1, 2, 3).reduceRight(_ + _))           -> 6,
    eval(Vector(1, 2, 3).reduceRight(_ + _))         -> 6,
    eval(Set(1, 2, 3).reduceRight(_ + _))            -> 6,
    eval(List(1, 2, 3).reduceRightOption(_ + _))     -> Some(6),
    eval(Vector(1, 2, 3).reduceRightOption(_ + _))   -> Some(6),
    eval(Set(1, 2, 3).reduceRightOption(_ + _))      -> Some(6),
    eval(List.empty[Int].reduceRightOption(_ + _))   -> None,
    eval(Vector.empty[Int].reduceRightOption(_ + _)) -> None,
    eval(Set.empty[Int].reduceRightOption(_ + _))    -> None,

    // String operations
    eval(List(1, 2, 3).mkString)                  -> "123",
    eval(Vector(1, 2, 3).mkString)                -> "123",
    eval(Set(1, 2, 3).mkString)                   -> "123",
    eval(List(1, 2, 3).mkString(","))             -> "1,2,3",
    eval(Vector(1, 2, 3).mkString(","))           -> "1,2,3",
    eval(Set(1, 2, 3).mkString(","))              -> "1,2,3",
    eval(List(1, 2, 3).mkString("[", ",", "]"))   -> "[1,2,3]",
    eval(Vector(1, 2, 3).mkString("[", ",", "]")) -> "[1,2,3]",
    eval(Set(1, 2, 3).mkString("[", ",", "]"))    -> "[1,2,3]",

    // Transpose operations
    eval(List(List(1, 2, 3), List(4, 5, 6)).transpose) -> List(List(1, 4), List(2, 5), List(3, 6)),

    // Zip operations
    eval(List(1, 2, 3).zipWithIndex) -> List((1, 0), (2, 1), (3, 2))
  )
