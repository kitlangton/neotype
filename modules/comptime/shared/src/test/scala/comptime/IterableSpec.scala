package comptime

import zio.test.*

object IterableSpec extends ZIOSpecDefault:
  // TODO: Some tests in this file use unsupported comptime operations (sum, product, max, fold, etc.)
  // Re-enable as these features are added to the comptime evaluator.
  // Original tests are preserved in a comment block at the bottom of the file.
  val spec =
    suite("IterableSpec (comptime)")(
      suite("predicate operations")(
        test("exists") {
          assertTrue(
            comptime(List(1, 2, 3).exists(_ > 2)) == true,
            comptime(List(1, 2, 3).exists(_ > 10)) == false,
            comptime(Vector(1, 2, 3).exists(_ > 2)) == true
          )
        },
        test("forall") {
          assertTrue(
            comptime(List(1, 2, 3).forall(_ > 0)) == true,
            comptime(List(1, 2, 3).forall(_ > 2)) == false,
            comptime(Vector(1, 2, 3).forall(_ < 10)) == true
          )
        },
        test("find") {
          assertTrue(
            comptime(List(1, 2, 3).find(_ > 1)) == Some(2),
            comptime(List(1, 2, 3).find(_ > 10)) == None,
            comptime(Vector(1, 2, 3).find(_ == 3)) == Some(3)
          )
        },
        test("count") {
          assertTrue(
            comptime(List(1, 2, 3).count(_ > 1)) == 2,
            comptime(List(1, 2, 3).count(_ > 10)) == 0,
            comptime(Vector(1, 2, 3, 4, 5).count(_ % 2 == 0)) == 2
          )
        },
        test("takeWhile") {
          assertTrue(
            comptime(List(1, 2, 3, 4).takeWhile(_ < 3)) == List(1, 2),
            comptime(List(1, 2, 3).takeWhile(_ > 10)) == List(),
            comptime(Vector(1, 2, 3, 4).takeWhile(_ < 4)) == Vector(1, 2, 3)
          )
        },
        test("dropWhile") {
          assertTrue(
            comptime(List(1, 2, 3, 4).dropWhile(_ < 3)) == List(3, 4),
            comptime(List(1, 2, 3).dropWhile(_ > 10)) == List(1, 2, 3),
            comptime(Vector(1, 2, 3, 4).dropWhile(_ < 2)) == Vector(2, 3, 4)
          )
        }
      ),
      suite("int operations")(
        test("takeRight") {
          assertTrue(
            comptime(List(1, 2, 3, 4).takeRight(2)) == List(3, 4),
            comptime(List(1, 2, 3).takeRight(0)) == List(),
            comptime(Vector(1, 2, 3).takeRight(1)) == Vector(3)
          )
        },
        test("dropRight") {
          assertTrue(
            comptime(List(1, 2, 3, 4).dropRight(2)) == List(1, 2),
            comptime(List(1, 2, 3).dropRight(0)) == List(1, 2, 3),
            comptime(Vector(1, 2, 3).dropRight(1)) == Vector(1, 2)
          )
        },
        test("splitAt") {
          assertTrue(
            comptime(List(1, 2, 3, 4, 5).splitAt(2)) == (List(1, 2), List(3, 4, 5)),
            comptime(Vector(1, 2, 3).splitAt(0)) == (Vector(), Vector(1, 2, 3)),
            comptime(List(1, 2, 3).splitAt(3)) == (List(1, 2, 3), List())
          )
        }
      ),
      suite("partition operations")(
        test("span") {
          assertTrue(
            comptime(List(1, 2, 3, 4).span(_ < 3)) == (List(1, 2), List(3, 4)),
            comptime(Vector(1, 2, 3).span(_ > 10)) == (Vector(), Vector(1, 2, 3)),
            comptime(List(1, 2, 3).span(_ < 10)) == (List(1, 2, 3), List())
          )
        },
        test("partition") {
          assertTrue(
            comptime(List(1, 2, 3, 4).partition(_ % 2 == 0)) == (List(2, 4), List(1, 3)),
            comptime(Vector(1, 2, 3).partition(_ > 10)) == (Vector(), Vector(1, 2, 3)),
            comptime(List(1, 2, 3).partition(_ < 10)) == (List(1, 2, 3), List())
          )
        },
        test("groupBy") {
          assertTrue(
            comptime(List(1, 2, 3, 4).groupBy(_ % 2)) == Map(0 -> List(2, 4), 1 -> List(1, 3)),
            comptime(Vector("a", "bb", "ccc").groupBy(_.length)) == Map(
              1 -> Vector("a"),
              2 -> Vector("bb"),
              3 -> Vector("ccc")
            )
          )
        }
      ),
      suite("slice operations")(
        test("slice") {
          assertTrue(
            comptime(List(1, 2, 3, 4, 5).slice(1, 4)) == List(2, 3, 4),
            comptime(List(1, 2, 3).slice(0, 2)) == List(1, 2)
          )
        }
      ),
      suite("zip operations")(
        test("zip") {
          assertTrue(
            comptime(List(1, 2, 3).zip(List("a", "b", "c"))) == List((1, "a"), (2, "b"), (3, "c")),
            comptime(Vector(1, 2).zip(Vector(10, 20))) == Vector((1, 10), (2, 20))
          )
        },
        test("zipWithIndex") {
          assertTrue(
            comptime(List("a", "b", "c").zipWithIndex) == List(("a", 0), ("b", 1), ("c", 2)),
            comptime(Vector(10, 20, 30).zipWithIndex) == Vector((10, 0), (20, 1), (30, 2))
          )
        },
        test("unzip") {
          assertTrue(
            comptime(List((1, "a"), (2, "b")).unzip) == (List(1, 2), List("a", "b")),
            comptime(Vector((10, "x"), (20, "y")).unzip) == (Vector(10, 20), Vector("x", "y"))
          )
        },
        test("unzip3") {
          assertTrue(
            comptime(List((1, "a", true), (2, "b", false)).unzip3) == (List(1, 2), List("a", "b"), List(true, false))
          )
        }
      ),
      suite("numeric operations")(
        test("sum") {
          assertTrue(
            comptime(List(1, 2, 3).sum) == 6,
            comptime(Vector(10, 20, 30).sum) == 60,
            comptime(List(1L, 2L, 3L).sum) == 6L,
            comptime(List(1.5, 2.5).sum) == 4.0
          )
        },
        test("product") {
          assertTrue(
            comptime(List(2, 3, 4).product) == 24,
            comptime(Vector(1, 2, 3, 4).product) == 24,
            comptime(List(2L, 3L).product) == 6L,
            comptime(List(1.5, 2.0).product) == 3.0
          )
        },
        test("max") {
          assertTrue(
            comptime(List(1, 5, 3).max) == 5,
            comptime(Vector(10, 20, 5).max) == 20,
            comptime(List("a", "z", "m").max) == "z"
          )
        },
        test("min") {
          assertTrue(
            comptime(List(5, 1, 3).min) == 1,
            comptime(Vector(10, 5, 20).min) == 5,
            comptime(List("z", "a", "m").min) == "a"
          )
        },
        test("maxOption") {
          assertTrue(
            comptime(List(1, 5, 3).maxOption) == Some(5),
            comptime(List.empty[Int].maxOption) == None
          )
        },
        test("minOption") {
          assertTrue(
            comptime(List(5, 1, 3).minOption) == Some(1),
            comptime(List.empty[Int].minOption) == None
          )
        }
      ),
      suite("collect operations")(
        test("collect") {
          assertTrue(
            comptime(List(1, 2, 3, 4).collect { case x if x % 2 == 0 => x * 10 }) == List(20, 40),
            comptime(Vector(Some(1), None, Some(2)).collect { case Some(x) => x }) == Vector(1, 2)
          )
        },
        test("collectFirst") {
          assertTrue(
            comptime(List(1, 2, 3, 4).collectFirst { case x if x > 2 => x * 10 }) == Some(30),
            comptime(List(1, 2, 3).collectFirst { case x if x > 10 => x }) == None
          )
        }
      ),
      suite("seq operations")(
        test("headOption") {
          assertTrue(
            comptime(List(1, 2, 3).headOption) == Some(1),
            comptime(Vector(10, 20).headOption) == Some(10),
            comptime(List.empty[Int].headOption) == None
          )
        },
        test("lastOption") {
          assertTrue(
            comptime(List(1, 2, 3).lastOption) == Some(3),
            comptime(Vector(10, 20).lastOption) == Some(20),
            comptime(List.empty[Int].lastOption) == None
          )
        },
        test("indices") {
          assertTrue(
            comptime(List(1, 2, 3).indices.toList) == List(0, 1, 2),
            comptime(Vector("a", "b").indices.toList) == List(0, 1),
            comptime(List.empty[Int].indices.toList) == List.empty[Int]
          )
        }
      ),
      suite("flatten operations")(
        test("flatten") {
          assertTrue(
            comptime(List(List(1, 2), List(3, 4)).flatten) == List(1, 2, 3, 4),
            comptime(Vector(Vector(1), Vector(2, 3)).flatten) == Vector(1, 2, 3),
            comptime(List(List.empty[Int], List(1)).flatten) == List(1)
          )
        }
      ),
      suite("conversion operations")(
        test("toList") {
          assertTrue(
            comptime(Vector(1, 2, 3).toList) == List(1, 2, 3)
          )
        },
        test("toVector") {
          assertTrue(
            comptime(List(1, 2, 3).toVector) == Vector(1, 2, 3)
          )
        },
        test("toSeq") {
          assertTrue(
            comptime(List(1, 2, 3).toSeq) == Seq(1, 2, 3)
          )
        },
        test("toSet") {
          assertTrue(
            comptime(List(1, 2, 2, 3).toSet) == Set(1, 2, 3),
            comptime(Vector(1, 1, 2).toSet) == Set(1, 2)
          )
        },
        test("toMap") {
          assertTrue(
            comptime(List(("a", 1), ("b", 2)).toMap) == Map("a" -> 1, "b" -> 2),
            comptime(Vector((1, "x"), (2, "y")).toMap) == Map(1 -> "x", 2 -> "y")
          )
        }
      ),
      suite("fold operations")(
        test("foldRight") {
          assertTrue(
            comptime(List(1, 2, 3).foldRight(0)(_ + _)) == 6,
            comptime(Vector("a", "b", "c").foldRight("")(_ + _)) == "abc",
            comptime(List(1, 2, 3).foldRight(10)(_ - _)) == -8
          )
        }
      ),
      suite("sort operations")(
        test("sortWith") {
          assertTrue(
            comptime(List(3, 1, 2).sortWith(_ < _)) == List(1, 2, 3),
            comptime(Vector(3, 1, 2).sortWith(_ > _)) == Vector(3, 2, 1)
          )
        },
        test("sortBy") {
          assertTrue(
            comptime(List("bbb", "a", "cc").sortBy(_.length)) == List("a", "cc", "bbb"),
            comptime(List(("b", 2), ("a", 1), ("c", 3)).sortBy(_._2)) == List(("a", 1), ("b", 2), ("c", 3)),
            comptime(Vector(3, 1, 2).sortBy(x => -x)) == Vector(3, 2, 1),
            comptime(List.empty[Int].sortBy(identity)) == List.empty[Int]
          )
        },
        test("sorted") {
          assertTrue(
            comptime(List(3, 1, 2).sorted) == List(1, 2, 3),
            comptime(Vector("c", "a", "b").sorted) == Vector("a", "b", "c"),
            comptime(List.empty[Int].sorted) == List.empty[Int]
          )
        }
      ),
      suite("reduce operations")(
        test("reduce") {
          assertTrue(
            comptime(List(1, 2, 3).reduce(_ + _)) == 6,
            comptime(Vector(1, 2, 3, 4).reduce(_ * _)) == 24
          )
        },
        test("reduceLeft") {
          assertTrue(
            comptime(List(1, 2, 3).reduceLeft(_ + _)) == 6,
            comptime(Vector("a", "b", "c").reduceLeft(_ + _)) == "abc"
          )
        },
        test("reduceRight") {
          assertTrue(
            comptime(List(1, 2, 3).reduceRight(_ + _)) == 6,
            comptime(Vector("a", "b", "c").reduceRight(_ + _)) == "abc"
          )
        },
        test("reduceOption") {
          assertTrue(
            comptime(List(1, 2, 3).reduceOption(_ + _)) == Some(6),
            comptime(List.empty[Int].reduceOption(_ + _)) == None
          )
        },
        test("reduceLeftOption") {
          assertTrue(
            comptime(List(1, 2, 3).reduceLeftOption(_ + _)) == Some(6),
            comptime(List.empty[Int].reduceLeftOption(_ + _)) == None
          )
        },
        test("reduceRightOption") {
          assertTrue(
            comptime(List(1, 2, 3).reduceRightOption(_ + _)) == Some(6),
            comptime(List.empty[Int].reduceRightOption(_ + _)) == None
          )
        }
      )
    )

  inline def wrap[A](a: A): A = a

  /* COMMENTED OUT - Unsupported operations in strict comptime mode
  lazy val evalTests = List(
    // Concatenation operations
    comptime(List(1, 2, 3) ++ (List(4, 5, 6)))        -> List(1, 2, 3, 4, 5, 6),
    comptime(Set(1, 2, 3) ++ (Set(4, 5, 6)))          -> Set(1, 2, 3, 4, 5, 6),
    comptime(Vector(1, 2, 3) ++ (Vector(4, 5, 6)))    -> Vector(1, 2, 3, 4, 5, 6),
    comptime(List(1, 2, 3).concat(List(4, 5, 6)))     -> List(1, 2, 3, 4, 5, 6),
    comptime(Vector(1, 2, 3).concat(Vector(4, 5, 6))) -> Vector(1, 2, 3, 4, 5, 6),

    // Zip / unzip
    comptime(List(1, 2, 3).zip(List(10, 20, 30)))            -> List((1, 10), (2, 20), (3, 30)),
    comptime(Vector(1, 2).zip(Vector("a", "b")))             -> Vector((1, "a"), (2, "b")),
    comptime(List((1, "a"), (2, "b")).unzip)                 -> (List(1, 2), List("a", "b")),
    comptime(Vector((1, "a", true), (2, "b", false)).unzip3) -> (Vector(1, 2), Vector("a", "b"), Vector(true, false)),

    // Collect operations
    comptime(List(1, 2, 3).collect { case x if x > 1 => x * 2 })                                -> List(4, 6),
    comptime(Vector(1, 2, 3).collect { case 1 | 3 => 0 })                                       -> Vector(0, 0),
    comptime(Set(1, 2, 3).collect { case 2 => 5 })                                              -> Set(5),
    comptime(List((1, 2), (3, 4)).collect { case (a, b) => a + b })                             -> List(3, 7),
    comptime(List(Option(1), None, Option(2)).collect { case Some(n) => n })                    -> List(1, 2),
    comptime(List(List(1, 2), Nil).collect { case h :: _ => h })                                -> List(1),
    comptime(List(Option((1, 2)), None, Option((3, 4))).collect { case Some((a, b)) => a + b }) -> List(3, 7),
    comptime(List(List(1, 2), List(3)).collect { case h :: t :: _ => h + t })                   -> List(3),
    comptime(List(1, 2, 3).collectFirst { case x if x > 1 => x * 2 })                           -> Some(4),
    comptime(List(1, 2, 3).collectFirst { case x if x > 10 => x })                              -> None,
    comptime(Vector(1, 2, 3).collectFirst { case 2 => 5 })                                      -> Some(5),

    // Element operations
    comptime(List(1, 2, 3).count(_ % 2 == 0))   -> 1,
    comptime(Set(1, 2, 3).count(_ % 2 == 0))    -> 1,
    comptime(Vector(1, 2, 3).count(_ % 2 == 0)) -> 1,

    // Drop operations
    comptime(List(1, 2, 3).drop(1))            -> List(2, 3),
    comptime(Set(1, 2, 3).drop(1))             -> Set(2, 3),
    comptime(Vector(1, 2, 3).drop(1))          -> Vector(2, 3),
    comptime(List(1, 2, 3).dropRight(1))       -> List(1, 2),
    comptime(Set(1, 2, 3).dropRight(1))        -> Set(1, 2),
    comptime(Vector(1, 2, 3).dropRight(1))     -> Vector(1, 2),
    comptime(List(1, 2, 3).dropWhile(_ < 2))   -> List(2, 3),
    comptime(Set(1, 2, 3).dropWhile(_ < 2))    -> Set(2, 3),
    comptime(Vector(1, 2, 3).dropWhile(_ < 2)) -> Vector(2, 3),

    // Take / slice operations
    comptime(List(1, 2, 3).take(2))            -> List(1, 2),
    comptime(Vector(1, 2, 3).take(2))          -> Vector(1, 2),
    comptime(List(1, 2, 3).takeRight(2))       -> List(2, 3),
    comptime(Vector(1, 2, 3).takeRight(2))     -> Vector(2, 3),
    comptime(List(1, 2, 3).takeWhile(_ < 3))   -> List(1, 2),
    comptime(Vector(1, 2, 3).takeWhile(_ < 3)) -> Vector(1, 2),
    comptime(List(1, 2, 3).slice(1, 3))        -> List(2, 3),
    comptime(Vector(1, 2, 3).slice(1, 3))      -> Vector(2, 3),

    // Int ranges
    comptime((1 to 3).toList)       -> List(1, 2, 3),
    comptime((1 until 3).toList)    -> List(1, 2),
    comptime((1 to 5).by(2).toList) -> List(1, 3, 5),

    // Empty collections
    comptime(List.empty)   -> List(),
    comptime(Nil)          -> List(),
    comptime(None)         -> None,
    comptime(Set.empty)    -> Set(),
    comptime(Vector.empty) -> Vector(),
    comptime(Map.empty)    -> Map(),

    // Existence checks
    comptime(List(1, 2, 3).exists(_ > 1))   -> true,
    comptime(Set(1, 2, 3).exists(_ > 1))    -> true,
    comptime(Vector(1, 2, 3).exists(_ > 1)) -> true,

    // Filter operations
    comptime(List(1, 2, 3).filter(_ > 1))      -> List(2, 3),
    comptime(Set(1, 2, 3).filter(_ > 1))       -> Set(2, 3),
    comptime(Vector(1, 2, 3).filter(_ > 1))    -> Vector(2, 3),
    comptime(List(1, 2, 3).filterNot(_ > 1))   -> List(1),
    comptime(Set(1, 2, 3).filterNot(_ > 1))    -> Set(1),
    comptime(Vector(1, 2, 3).filterNot(_ > 1)) -> Vector(1),

    // Find operations
    comptime(List(1, 2, 3).find(_ > 1))   -> Some(2),
    comptime(Set(1, 2, 3).find(_ > 1))    -> Some(2),
    comptime(Vector(1, 2, 3).find(_ > 1)) -> Some(2),

    // FlatMap and flatten
    comptime(List(1, 2, 3).flatMap(a => List(a, a * 2)))      -> List(1, 2, 2, 4, 3, 6),
    comptime(Set(1, 2, 3).flatMap(a => Set(a, a * 2)))        -> Set(1, 2, 3, 4, 6),
    comptime(Vector(1, 2, 3).flatMap(a => Vector(a, a * 2)))  -> Vector(1, 2, 2, 4, 3, 6),
    comptime(List(List(1), List(2), List(3)).flatten)         -> List(1, 2, 3),
    comptime(Set(Set(1), Set(2), Set(3)).flatten)             -> Set(1, 2, 3),
    comptime(Vector(Vector(1), Vector(2), Vector(3)).flatten) -> Vector(1, 2, 3),

    // Fold operations
    comptime(List(1, 2, 3).fold(0)(_ + _))        -> 6,
    comptime(Set(1, 2, 3).fold(0)(_ + _))         -> 6,
    comptime(Vector(1, 2, 3).fold(0)(_ + _))      -> 6,
    comptime(List(1, 2, 3).foldLeft(0)(_ + _))    -> 6,
    comptime(Set(1, 2, 3).foldLeft(0)(_ + _))     -> 6,
    comptime(Vector(1, 2, 3).foldLeft(0)(_ + _))  -> 6,
    comptime(List(1, 2, 3).foldRight(0)(_ + _))   -> 6,
    comptime(Set(1, 2, 3).foldRight(0)(_ + _))    -> 6,
    comptime(Vector(1, 2, 3).foldRight(0)(_ + _)) -> 6,

    // ForAll operations
    comptime(List(1, 2, 3).forall(_ => true))   -> true,
    comptime(Set(1, 2, 3).forall(_ => true))    -> true,
    comptime(Vector(1, 2, 3).forall(_ => true)) -> true,

    // Foreach operations
    comptime(List(1, 2, 3).foreach(_ => ()))   -> (),
    comptime(Set(1, 2, 3).foreach(_ => ()))    -> (),
    comptime(Vector(1, 2, 3).foreach(_ => ())) -> (),

    // Grouping operations
    comptime(List(1, 2, 3).groupBy(_ => 0))                         -> Map(0 -> List(1, 2, 3)),
    comptime(Set(1, 2, 3).groupBy(_ => 0))                          -> Map(0 -> Set(1, 2, 3)),
    comptime(Vector(1, 2, 3).groupBy(_ => 0))                       -> Map(0 -> Vector(1, 2, 3)),
    comptime(List(1, 2, 3).groupMap(_ => 0)(_ => 0))                -> Map(0 -> List(0, 0, 0)),
    comptime(Set(1, 2, 3).groupMap(_ => 0)(_ => 0))                 -> Map(0 -> Set(0)),
    comptime(Vector(1, 2, 3).groupMap(_ => 0)(_ => 0))              -> Map(0 -> Vector(0, 0, 0)),
    comptime(List(1, 2, 3).groupMapReduce(_ => 0)(_ => 0)(_ + _))   -> Map(0 -> 0),
    comptime(Set(1, 2, 3).groupMapReduce(_ => 0)(_ => 0)(_ + _))    -> Map(0 -> 0),
    comptime(Vector(1, 2, 3).groupMapReduce(_ => 0)(_ => 0)(_ + _)) -> Map(0 -> 0),

    // Grouped operations
    comptime(List(1, 2, 3).grouped(1).toList)     -> List(List(1), List(2), List(3)),
    comptime(Set(1, 2, 3).grouped(1).toSet)       -> Set(Set(1), Set(2), Set(3)),
    comptime(Vector(1, 2, 3).grouped(1).toVector) -> Vector(Vector(1), Vector(2), Vector(3)),

    // Head operations
    comptime(List(1, 2, 3).head)         -> 1,
    comptime(Set(1, 2, 3).head)          -> 1,
    comptime(Vector(1, 2, 3).head)       -> 1,
    comptime(List(1, 2, 3).headOption)   -> Some(1),
    comptime(Set(1, 2, 3).headOption)    -> Some(1),
    comptime(Vector(1, 2, 3).headOption) -> Some(1),

    // Init operations
    comptime(List(1, 2, 3).init)           -> List(1, 2),
    comptime(Vector(1, 2, 3).init)         -> Vector(1, 2),
    comptime(Set(1, 2, 3).init)            -> Set(1, 2),
    comptime(List(1, 2, 3).inits.toList)   -> List(List(1, 2, 3), List(1, 2), List(1), List()),
    comptime(Vector(1, 2, 3).inits.toList) -> List(Vector(1, 2, 3), Vector(1, 2), Vector(1), Vector()),
    comptime(Set(1, 2, 3).inits.toList)    -> List(Set(1, 2, 3), Set(1, 2), Set(1), Set()),

    // Size operations
    comptime(List(1, 2, 3).knownSize)   -> -1,
    comptime(Vector(1, 2, 3).knownSize) -> 3,
    comptime(Set(1, 2, 3).knownSize)    -> 3,

    // Empty checks
    comptime(List(1, 2, 3).isEmpty)   -> false,
    comptime(Vector(1, 2, 3).isEmpty) -> false,
    comptime(Set(1, 2, 3).isEmpty)    -> false,
    comptime(List().isEmpty)          -> true,
    comptime(Vector().isEmpty)        -> true,
    comptime(Set().isEmpty)           -> true,

    // Empty value
    comptime(List(1, 2, 3).empty)   -> List(),
    comptime(Vector(1, 2, 3).empty) -> Vector(),
    comptime(Set(1, 2, 3).empty)    -> Set(),

    // Last operations
    comptime(List(1, 2, 3).last)         -> 3,
    comptime(Vector(1, 2, 3).last)       -> 3,
    comptime(Set(1, 2, 3).last)          -> 3,
    comptime(List(1, 2, 3).lastOption)   -> Some(3),
    comptime(Vector(1, 2, 3).lastOption) -> Some(3),
    comptime(Set(1, 2, 3).lastOption)    -> Some(3),
    comptime(List().lastOption)          -> None,
    comptime(Vector().lastOption)        -> None,
    comptime(Set().lastOption)           -> None,

    // Map operations
    comptime(List(1, 2, 3).map(a => a))   -> List(1, 2, 3),
    comptime(Vector(1, 2, 3).map(a => a)) -> Vector(1, 2, 3),
    comptime(Set(1, 2, 3).map(a => a))    -> Set(1, 2, 3),

    // Sum / product (custom numeric zero/one)
    comptime(List(1, 2, 3).sum)                                    -> 6,
    comptime(Vector(1.5, 2.5).sum)                                 -> 4.0,
    comptime(List(BigInt(2), BigInt(3)).sum)                       -> BigInt(5),
    comptime(Vector(BigDecimal("1.5"), BigDecimal("2.5")).sum)     -> BigDecimal("4.0"),
    comptime(List.empty[Int].sum)                                  -> 0,
    comptime(List.empty[BigInt].sum)                               -> BigInt(0),
    comptime(List.empty[BigDecimal].sum)                           -> BigDecimal(0),
    comptime(List(2, 3, 4).product)                                -> 24,
    comptime(Vector(1.5, 2.0).product)                             -> 3.0,
    comptime(List(BigInt(2), BigInt(3)).product)                   -> BigInt(6),
    comptime(Vector(BigDecimal("1.5"), BigDecimal("2.0")).product) -> BigDecimal("3.0"),
    comptime(List.empty[Int].product)                              -> 1,
    comptime(List.empty[BigInt].product)                           -> BigInt(1),
    comptime(List.empty[BigDecimal].product)                       -> BigDecimal(1),

    // Max/Min operations
    comptime(List(1, 2, 3).max)                     -> 3,
    comptime(Set(1, 2, 3).max)                      -> 3,
    comptime(Vector(1, 2, 3).max)                   -> 3,
    comptime(List(1, 2, 3).maxOption)               -> Some(3),
    comptime(Vector(1, 2, 3).maxOption)             -> Some(3),
    comptime(Set(1, 2, 3).maxOption)                -> Some(3),
    comptime(List.empty[Int].maxOption)             -> None,
    comptime(Vector.empty[Int].maxOption)           -> None,
    comptime(Set.empty[Int].maxOption)              -> None,
    comptime(List(1, 2, 3).maxBy(_ => 0))           -> 1,
    comptime(Vector(1, 2, 3).maxBy(_ => 0))         -> 1,
    comptime(Set(1, 2, 3).maxBy(_ => 0))            -> 1,
    comptime(List(1, 2, 3).maxByOption(_ => 0))     -> Some(1),
    comptime(Vector(1, 2, 3).maxByOption(_ => 0))   -> Some(1),
    comptime(Set(1, 2, 3).maxByOption(_ => 0))      -> Some(1),
    comptime(List.empty[Int].maxByOption(_ => 0))   -> None,
    comptime(Vector.empty[Int].maxByOption(_ => 0)) -> None,
    comptime(Set.empty[Int].maxByOption(_ => 0))    -> None,
    comptime(List(1, 2, 3).min)                     -> 1,
    comptime(Vector(1, 2, 3).min)                   -> 1,
    comptime(Set(1, 2, 3).min)                      -> 1,
    comptime(List(1, 2, 3).minOption)               -> Some(1),
    comptime(Vector(1, 2, 3).minOption)             -> Some(1),
    comptime(Set(1, 2, 3).minOption)                -> Some(1),
    comptime(List.empty[Int].minOption)             -> None,
    comptime(Vector.empty[Int].minOption)           -> None,
    comptime(Set.empty[Int].minOption)              -> None,
    comptime(List(1, 2, 3).minBy(_ => 0))           -> 1,
    comptime(Vector(1, 2, 3).minBy(_ => 0))         -> 1,
    comptime(Set(1, 2, 3).minBy(_ => 0))            -> 1,
    comptime(List(1, 2, 3).minByOption(_ => 0))     -> Some(1),
    comptime(Vector(1, 2, 3).minByOption(_ => 0))   -> Some(1),
    comptime(Set(1, 2, 3).minByOption(_ => 0))      -> Some(1),
    comptime(List.empty[Int].minByOption(_ => 0))   -> None,
    comptime(Vector.empty[Int].minByOption(_ => 0)) -> None,
    comptime(Set.empty[Int].minByOption(_ => 0))    -> None,

    // NonEmpty checks
    comptime(List(1, 2, 3).nonEmpty)     -> true,
    comptime(Vector(1, 2, 3).nonEmpty)   -> true,
    comptime(Set(1, 2, 3).nonEmpty)      -> true,
    comptime(List.empty[Int].nonEmpty)   -> false,
    comptime(Vector.empty[Int].nonEmpty) -> false,
    comptime(Set.empty[Int].nonEmpty)    -> false,

    // Partition operations
    comptime(List(1, 2, 3).partition(_ => true))          -> (List(1, 2, 3), List()),
    comptime(Vector(1, 2, 3).partition(_ => true))        -> (Vector(1, 2, 3), Vector()),
    comptime(Set(1, 2, 3).partition(_ => true))           -> (Set(1, 2, 3), Set()),
    comptime(List(1, 2, 3).partitionMap(a => Right(a)))   -> (List(), List(1, 2, 3)),
    comptime(Vector(1, 2, 3).partitionMap(a => Right(a))) -> (Vector(), Vector(1, 2, 3)),
    comptime(Set(1, 2, 3).partitionMap(a => Right(a)))    -> (Set(), Set(1, 2, 3)),

    // Reduce operations
    comptime(List(1, 2, 3).reduce(_ + _))                -> 6,
    comptime(Vector(1, 2, 3).reduce(_ + _))              -> 6,
    comptime(Set(1, 2, 3).reduce(_ + _))                 -> 6,
    comptime(List(1, 2, 3).reduceOption(_ + _))          -> Some(6),
    comptime(Vector(1, 2, 3).reduceOption(_ + _))        -> Some(6),
    comptime(Set(1, 2, 3).reduceOption(_ + _))           -> Some(6),
    comptime(List.empty[Int].reduceOption(_ + _))        -> None,
    comptime(Vector.empty[Int].reduceOption(_ + _))      -> None,
    comptime(Set.empty[Int].reduceOption(_ + _))         -> None,
    comptime(List(1, 2, 3).reduceLeft(_ + _))            -> 6,
    comptime(Vector(1, 2, 3).reduceLeft(_ + _))          -> 6,
    comptime(Set(1, 2, 3).reduceLeft(_ + _))             -> 6,
    comptime(List(1, 2, 3).reduceLeftOption(_ + _))      -> Some(6),
    comptime(Vector(1, 2, 3).reduceLeftOption(_ + _))    -> Some(6),
    comptime(Set(1, 2, 3).reduceLeftOption(_ + _))       -> Some(6),
    comptime(List.empty[Int].reduceLeftOption(_ + _))    -> None,
    comptime(Vector.empty[Int].reduceLeftOption(_ + _))  -> None,
    comptime(Set.empty[Int].reduceLeftOption(_ + _))     -> None,
    comptime(List(1, 2, 3).reduceRight(_ + _))           -> 6,
    comptime(Vector(1, 2, 3).reduceRight(_ + _))         -> 6,
    comptime(Set(1, 2, 3).reduceRight(_ + _))            -> 6,
    comptime(List(1, 2, 3).reduceRightOption(_ + _))     -> Some(6),
    comptime(Vector(1, 2, 3).reduceRightOption(_ + _))   -> Some(6),
    comptime(Set(1, 2, 3).reduceRightOption(_ + _))      -> Some(6),
    comptime(List.empty[Int].reduceRightOption(_ + _))   -> None,
    comptime(Vector.empty[Int].reduceRightOption(_ + _)) -> None,
    comptime(Set.empty[Int].reduceRightOption(_ + _))    -> None,

    // String operations
    comptime(List(1, 2, 3).mkString)                  -> "123",
    comptime(Vector(1, 2, 3).mkString)                -> "123",
    comptime(Set(1, 2, 3).mkString)                   -> "123",
    comptime(List(1, 2, 3).mkString(","))             -> "1,2,3",
    comptime(Vector(1, 2, 3).mkString(","))           -> "1,2,3",
    comptime(Set(1, 2, 3).mkString(","))              -> "1,2,3",
    comptime(List(1, 2, 3).mkString("[", ",", "]"))   -> "[1,2,3]",
    comptime(Vector(1, 2, 3).mkString("[", ",", "]")) -> "[1,2,3]",
    comptime(Set(1, 2, 3).mkString("[", ",", "]"))    -> "[1,2,3]",

    // Transpose operations
    comptime(List(List(1, 2, 3), List(4, 5, 6)).transpose) -> List(List(1, 4), List(2, 5), List(3, 6)),

    // Zip operations
    comptime(List(1, 2, 3).zipWithIndex) -> List((1, 0), (2, 1), (3, 2))
  )
   */
