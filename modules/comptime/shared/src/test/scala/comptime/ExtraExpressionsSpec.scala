package comptime

import zio.test.*

object ExtraExpressionsSpec extends ZIOSpecDefault:
  val spec =
    suite("ExtraExpressionsSpec (comptime)")(
      evalTests.map { case (actual, expected) =>
        test(s"comptime($actual) == $expected") {
          assertTrue(actual == expected)
        }
      }
    )

  case class Person(name: String, age: Int)

  lazy val evalTests = List(
    // Match expressions (literal / wildcard / variable / guard / alternatives)
    comptime {
      val x = 1
      x match
        case 1 => "one"
        case _ => "other"
    } -> "one",
    comptime {
      val x = 2
      x match
        case 1 | 2 => "small"
        case _     => "other"
    } -> "small",
    // Match expressions (nested alternatives without bindings)
    comptime {
      val x: Option[Option[Any]] = Some(Some("abc"))
      x match
        case Some(Some(_: String) | None) => 1
        case _                            => 0
    } -> 1,
    comptime {
      val x: Option[Option[Any]] = Some(None)
      x match
        case Some(Some(_: String) | None) => 1
        case _                            => 0
    } -> 1,
    comptime {
      val x: Option[Option[Any]] = Some(Some(123))
      x match
        case Some(Some(_: String) | None) => 1
        case _                            => 0
    } -> 0,
    comptime {
      val x = 2
      x match
        case n if n > 1 => n + 1
        case _          => 0
    } -> 3,
    comptime {
      val x: Option[(Int, Int)] = Some((1, 2))
      x match
        case Some((a, b)) if a < b => b - a
        case _                     => 0
    } -> 1,

    // Match expressions (typed patterns: `case _: T` / `case x: T`)
    comptime {
      val x: Any = "abc"
      x match
        case _: String => 1
        case _         => 0
    } -> 1,
    comptime {
      val x: Any = "abc"
      x match
        case s: String => s.length
        case _         => 0
    } -> 3,
    comptime {
      val x: Any = 123
      x match
        case s: String => s.length
        case _         => 0
    } -> 0,

    // Match expressions (unapply patterns: Option / Tuple / List)
    comptime {
      val x: Option[Int] = Some(1)
      x match
        case Some(n) => n + 1
        case None    => 0
    } -> 2,
    comptime {
      val x: Option[Int] = None
      x match
        case Some(n) => n + 1
        case None    => 0
    } -> 0,
    comptime {
      val x = (1, "a")
      x match
        case (a, b) => a.toString + b
    } -> "1a",
    comptime {
      val x = (1, 2, 3)
      x match
        case (a, b, c) => a + b + c
    } -> 6,

    // :: (cons) pattern matching
    comptime {
      val xs = List(1, 2, 3)
      xs match
        case h :: t => h + t.size
        case Nil    => 0
    } -> 3,
    comptime {
      val xs = List((1, 2))
      xs match
        case (a, b) :: _ => a * 10 + b
        case _           => 0
    } -> 12,
    comptime {
      val x: Any = List(1, 2, 3)
      x match
        case (h: Int) :: t => h + t.size
        case _             => 0
    } -> 3,

    // ═══════════════════════════════════════════════════════════════════
    // NOTE: sum/product, BigInt/BigDecimal, :: pattern matching NOW WORK.
    // Remaining unsupported: String|Boolean lifting, named args, local defs.
    // ═══════════════════════════════════════════════════════════════════

    // Collection sum/product (these work in other test files already)
    comptime(List(1, 2, 3).sum)     -> 6,
    comptime(List(2, 3, 4).product) -> 24,

    // Collection find/takeWhile/dropWhile/dropRight/takeRight
    comptime(List(1, 2, 3).find(_ > 2))         -> Some(3),
    comptime(List(1, 2, 3, 4).takeWhile(_ < 3)) -> List(1, 2),
    comptime(List(1, 2, 3, 4).dropWhile(_ < 3)) -> List(3, 4),
    comptime(List(1, 2, 3, 4).dropRight(2))     -> List(1, 2),
    comptime(List(1, 2, 3, 4).takeRight(2))     -> List(3, 4),

    // Collection collect/collectFirst
    comptime(List(1, 2, 3).collect { case n if n % 2 == 0 => n * 10 }) -> List(20),
    comptime(List(1, 2, 3).collectFirst { case n if n > 1 => n })      -> Some(2),

    // filter chain works
    comptime(List(1, 2, 3).filter(_ > 1).map(_ * 2)) -> List(4, 6),

    // withFilter for for-comprehensions
    comptime(List(1, 2, 3).withFilter(_ > 1).map(_ * 2)) -> List(4, 6),
    comptime(for (x <- Option(1) if x > 0) yield x + 1)  -> Some(2)
  )

  /* COMMENTED OUT TESTS:

      comptime {
        val xs = List((1, 2), (3, 4))
        xs match
          case List((a, b), rest*) => a + b + rest.size
          case _                   => 0
      } -> 4,

      // Match expressions (unapplySeq patterns: List(a, b))
      comptime {
        val xs = List(1, 2)
        xs match
          case List(a, b) => a + b
          case _          => 0
      } -> 3,
      comptime {
        val x: Any = List(1, 2)
        x match
          case List(a: Int, b: Int) => a + b
          case _                    => 0
      } -> 3,
      comptime {
        val x: Any = List(1, 2, 3)
        x match
          case List(a: Int, rest*) => a + rest.size
          case _                   => 0
      } -> 3,

      comptime {
        val x: Any = Vector(1, 2)
        x match
          case Vector(a: Int, b: Int) => a + b
          case _                      => 0
      } -> 3,

      // TODO: SeqFactory$.Delegate.apply is not supported in strict comptime
      // comptime {
      //   val x: Any = Seq(1, 2): Seq[Int]
      //   x match
      //     case Seq(a: Int, b: Int) => a + b
      //     case _                   => 0
      // } -> 3,

      comptime {
        val xs = List(1)
        xs match
          case List(a, b) => a + b
          case _          => 0
      } -> 0,
      comptime {
        val xs = List(1, 2, 3)
        xs match
          case List(a, b, _*) => a + b
          case _              => 0
      } -> 3,
      comptime {
        val xs = List(1, 2)
        xs match
          case List(a, b, _*) => a + b
          case _              => 0
      } -> 3,

      // List sum/product
      comptime {
        val xs = List(1, 2, 3)
        xs.sum
      } -> 6,
      comptime {
        val xs = List(BigInt(2), BigInt(3))
        xs.sum
      } -> BigInt(5),
      comptime {
        val xs: Vector[Double] = Vector(1.5, 2.5)
        xs.sum
      } -> 4.0,
      comptime {
        val xs: Vector[BigDecimal] = Vector(BigDecimal("1.5"), BigDecimal("2.5"))
        xs.sum
      } -> BigDecimal("4.0"),
      comptime {
        val xs = List.empty[Int]
        xs.sum
      } -> 0,
      comptime {
        val xs = List(2, 3, 4)
        xs.product
      } -> 24,
      comptime {
        val xs = List(BigInt(2), BigInt(3))
        xs.product
      } -> BigInt(6),
      comptime {
        val xs: Vector[Double] = Vector(1.5, 2.0)
        xs.product
      } -> 3.0,
      comptime {
        val xs: Vector[BigDecimal] = Vector(BigDecimal("1.5"), BigDecimal("2.0"))
        xs.product
      } -> BigDecimal("3.0"),

      // TODO: `slice` with named arguments is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3)
      //   xs.slice(until = 2, from = 1)
      // } -> List(2),

      // TODO: local `def` in comptime blocks is not supported in strict comptime
      // comptime {
      //   def mk(a: Int, b: Int): Int = a * 10 + b
      //   mk(b = 2, a = 1)
      // } -> 12,

      // Union types (String | Boolean) are now supported
      comptime {
        val input = "   "
        if input.isBlank then "Must not be empty" else true
      } -> "Must not be empty",

      comptime {
        val input = "ok"
        if input.isBlank then "Must not be empty" else true
      } -> true,

      comptime {
        val input = "550e8400-e29b-41d4-a716-446655440000"
        if !scala.util.Try(java.util.UUID.fromString(input).toString).toOption.contains(input.toLowerCase)
        then "Must be a valid UUID"
        else true
      } -> true,

      comptime {
        val input = "not-a-uuid"
        if !scala.util.Try(java.util.UUID.fromString(input).toString).toOption.contains(input.toLowerCase)
        then "Must be a valid UUID"
        else true
      } -> "Must be a valid UUID",

      comptime {
        val xs = List.empty[Int]
        xs.product
      } -> 1,
      // Option sum/product tests moved to OptionSpec.scala

      // TODO: splitAt is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4)
      //   xs.splitAt(2)
      // } -> (List(1, 2), List(3, 4)),

      // TODO: span is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4)
      //   xs.span(_ < 3)
      // } -> (List(1, 2), List(3, 4)),

      // TODO: zipAll is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2)
      //   xs.zipAll(List(10), 0, 99)
      // } -> List((1, 10), (2, 99)),

      // TODO: scanLeft is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3)
      //   xs.scanLeft(0)(_ + _)
      // } -> List(0, 1, 3, 6),

      // TODO: scanRight is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3)
      //   xs.scanRight(0)(_ + _)
      // } -> List(6, 5, 3, 0),

      // TODO: sliding is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4)
      //   xs.sliding(2).toList
      // } -> List(List(1, 2), List(2, 3), List(3, 4)),

      // TODO: sliding is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4)
      //   xs.sliding(2, 2).toList
      // } -> List(List(1, 2), List(3, 4)),

      // TODO: zip is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2)
      //   xs.zip(List(3, 4))
      // } -> List((1, 3), (2, 4)),

      // TODO: unzip is not supported in strict comptime
      // comptime {
      //   val xs = List((1, "a"), (2, "b"))
      //   xs.unzip
      // } -> (List(1, 2), List("a", "b")),

      // TODO: unzip3 is not supported in strict comptime
      // comptime {
      //   val xs = List((1, "a", true), (2, "b", false))
      //   xs.unzip3
      // } -> (List(1, 2), List("a", "b"), List(true, false)),

      // TODO: grouped is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4, 5)
      //   xs.grouped(2).toList
      // } -> List(List(1, 2), List(3, 4), List(5)),

      // TODO: partition is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4)
      //   xs.partition(_ % 2 == 0)
      // } -> (List(2, 4), List(1, 3)),

      // TODO: groupBy is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4)
      //   xs.groupBy(_ % 2)
      // } -> Map(1 -> List(1, 3), 0 -> List(2, 4)),

      comptime {
        val xs = List(1, 2, 3)
        xs.collect { case n if n % 2 == 0 => n * 10 }
      } -> List(20),
      comptime {
        val xs = List(1, 2, 3)
        xs.collectFirst { case n if n > 1 => n }
      } -> Some(2),

      comptime {
        val xs = List(1, 2, 3)
        xs.foldLeft(0)(_ + _)
      } -> 6,

      // TODO: foldRight is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3)
      //   xs.foldRight(0)(_ + _)
      // } -> 6,

      // TODO: groupMap is not supported in strict comptime
      // comptime {
      //   val xs = List("a", "ab", "b")
      //   xs.groupMap(_(0))(_.length)
      // } -> Map('a' -> List(1, 2), 'b' -> List(1)),

      // TODO: groupMapReduce is not supported in strict comptime
      // comptime {
      //   val xs = List("a", "ab", "b")
      //   xs.groupMapReduce(_(0))(_.length)(_ + _)
      // } -> Map('a' -> 3, 'b' -> 1),

      comptime {
        val s = "abc"
        s.head
      } -> 'a',
      comptime {
        val s = "abc"
        s.last
      } -> 'c',
      comptime {
        val s = "abc"
        s.headOption
      } -> Some('a'),
      comptime {
        val s = ""
        s.headOption
      } -> None,
      comptime {
        val s = ""
        s.lastOption
      } -> None,
      comptime {
        val s = "hello"
        s.tail
      } -> "ello",
      comptime {
        val s = "hello"
        s.init
      } -> "hell",
      comptime {
        val s = "hello"
        s.startsWith("he")
      } -> true,
      comptime {
        val s = "hello"
        s.endsWith("lo")
      } -> true,
      comptime {
        val s = "hello"
        s.contains("ell")
      } -> true,
      comptime {
        val s = "hello"
        s.contains('e')
      } -> true,
      comptime {
        val s = "hello"
        s.indexOf('e')
      } -> 1,
      comptime {
        val s = "hello"
        s.indexOf("ll")
      } -> 2,
      comptime {
        val s = "hello"
        s.indexOf('l', 3)
      } -> 3,
      comptime {
        val s = "hello"
        s.lastIndexOf('l')
      } -> 3,
      comptime {
        val s = "hello"
        s.lastIndexOf("l", 2)
      } -> 2,

      comptime {
        val xs = List(1, 2, 3, 4)
        xs.takeWhile(_ < 3)
      } -> List(1, 2),
      comptime {
        val xs = List(1, 2, 3, 4)
        xs.dropWhile(_ < 3)
      } -> List(3, 4),

      comptime {
        val xs = List(1, 2, 3)
        xs.filterNot(_ == 2)
      } -> List(1, 3),

      comptime {
        val xs = List(1, 2, 3)
        xs.find(_ > 2)
      } -> Some(3),

      comptime {
        val xs = List(1, 2, 3)
        xs.lastOption
      } -> Some(3),

      comptime {
        val xs = List(1, 2, 3, 4)
        xs.dropRight(2)
      } -> List(1, 2),
      comptime {
        val xs = List(1, 2, 3, 4)
        xs.takeRight(2)
      } -> List(3, 4),

      comptime {
        val xs = List("a", "b")
        xs.zipWithIndex
      } -> List(("a", 0), ("b", 1)),
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.map(_ * 2).toList
      } -> List(2, 4, 6),
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.filter(_ > 1).take(1).toList
      } -> List(2),
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.size
      } -> 3,
      comptime {
        val it = List(1).iterator
        (it.nextOption, it.nextOption)
      } -> (Some(1), None),
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.flatMap(n => List(n, n)).toList
      } -> List(1, 1, 2, 2, 3, 3),
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.collect { case n if n % 2 == 1 => n * 10 }.toList
      } -> List(10, 30),

      // TODO: collectFirst on iterator is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3)
      //   xs.iterator.collectFirst { case n if n > 2 => n }
      // } -> Some(3),

      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.filterNot(_ == 2).toList
      } -> List(1, 3),
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.takeWhile(_ < 3).toList
      } -> List(1, 2),
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.dropWhile(_ < 3).toList
      } -> List(3),

      // TODO: partition on iterator is not supported in strict comptime
      // comptime {
      //   val xs     = List(1, 2, 3, 4)
      //   val (a, b) = xs.iterator.partition(_ % 2 == 0)
      //   (a.toList, b.toList)
      // } -> (List(2, 4), List(1, 3)),

      // TODO: span on iterator is not supported in strict comptime
      // comptime {
      //   val xs     = List(1, 2, 3, 4)
      //   val (a, b) = xs.iterator.span(_ < 3)
      //   (a.toList, b.toList)
      // } -> (List(1, 2), List(3, 4)),

      // Iterator find
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.find(_ > 2)
      } -> Some(3),

      comptime {
        val xs = List(1, 2, 3, 4)
        xs.iterator.slice(1, 3).toList
      } -> List(2, 3),

      // TODO: `sum` on iterator is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4)
      //   xs.iterator.grouped(2).map(_.sum).toList
      // } -> List(3, 7),

      // TODO: `sum` on iterator is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4, 5)
      //   xs.iterator.sliding(2, 1).map(_.sum).toList
      // } -> List(3, 5, 7, 9),

      // TODO: `sum` on iterator is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3, 4, 5)
      //   xs.iterator.sliding(2, 2).map(_.sum).toList
      // } -> List(3, 7, 5),

      comptime {
        val xs = List(1, 2)
        xs.iterator.zip(List("a", "b", "c")).toList
      } -> List((1, "a"), (2, "b")),
      comptime {
        val xs = List(1, 2)
        xs.iterator.zipAll(List("a"), 0, "x").toList
      } -> List((1, "a"), (2, "x")),
      comptime {
        val xs = List(1, 2)
        xs.iterator.zipWithIndex.toList
      } -> List((1, 0), (2, 1)),
      comptime {
        val xs = List(1, 2, 1)
        xs.distinct
      } -> List(1, 2),
      comptime {
        val xs = Vector(1, 2, 3)
        xs.reverse
      } -> Vector(3, 2, 1),

      // TODO: Seq(...) construction via SeqFactory on Any type is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3)
      //   xs.iterator.toSeq
      // } -> Seq(1, 2, 3),

      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.toIndexedSeq.toVector
      } -> Vector(1, 2, 3),

      // Iterator mkString
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.mkString(",")
      } -> "1,2,3",

      // Iterator reduce
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.reduce(_ + _)
      } -> 6,

      // Iterator reduceOption
      comptime {
        val xs = List.empty[Int]
        xs.iterator.reduceOption(_ + _)
      } -> None,

      // Iterator exists
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.exists(_ > 2)
      } -> true,

      // Iterator count
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.count(_ % 2 == 1)
      } -> 2,

      // Iterator forall
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.forall(_ > 0)
      } -> true,

      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.toVector
      } -> Vector(1, 2, 3),
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.toSet
      } -> Set(1, 2, 3),

      // TODO: toMap on iterator is not supported in strict comptime
      // comptime {
      //   val xs = List(1 -> "a", 2 -> "b")
      //   xs.iterator.toMap
      // } -> Map(1 -> "a", 2 -> "b"),

      // Iterator sum
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.sum
      } -> 6,

      comptime {
        val xs = List(BigDecimal("1.5"), BigDecimal("2.5"))
        xs.iterator.sum
      } -> BigDecimal("4.0"),

      comptime {
        val xs = List(BigInt(2), BigInt(3))
        xs.iterator.sum
      } -> BigInt(5),

      comptime {
        val xs = List.empty[Int]
        xs.iterator.sum
      } -> 0,

      // Iterator product
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.product
      } -> 6,

      comptime {
        val xs = List(BigDecimal("1.5"), BigDecimal("2.0"))
        xs.iterator.product
      } -> BigDecimal("3.0"),

      comptime {
        val xs = List(BigInt(2), BigInt(3))
        xs.iterator.product
      } -> BigInt(6),

      comptime {
        val xs = List.empty[Int]
        xs.iterator.product
      } -> 1,

      // Iterator max/min
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.max
      } -> 3,

      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.min
      } -> 1,

      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.maxOption
      } -> Some(3),

      comptime {
        val xs = List.empty[Int]
        xs.iterator.minOption
      } -> None,

      // Iterator maxBy/minBy
      comptime {
        val xs = List("a", "bbb", "cc")
        xs.iterator.maxBy(_.length)
      } -> "bbb",
      comptime {
        val xs = List("a", "bbb", "cc")
        xs.iterator.minBy(_.length)
      } -> "a",
      comptime {
        val xs = List("a", "bbb", "cc")
        xs.iterator.maxByOption(_.length)
      } -> Some("bbb"),
      comptime {
        val xs = List.empty[String]
        xs.iterator.minByOption(_.length)
      } -> None,

      // Iterator mkString with 3 args
      comptime {
        val xs = List(1, 2, 3)
        xs.iterator.mkString("[", ",", "]")
      } -> "[1,2,3]",

      // TODO: foldLeft on iterator is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3)
      //   xs.iterator.foldLeft(0)(_ + _)
      // } -> 6,

      // TODO: next on iterator is not supported in strict comptime
      // comptime {
      //   val it = List(1, 2).iterator
      //   it.next() + it.next()
      // } -> 3,

      // TODO: Map.keys is not supported in strict comptime
      // comptime {
      //   val m = Map(1 -> "a", 2 -> "b")
      //   m.keys.toList.sorted
      // } -> List(1, 2),

      // TODO: Map.values is not supported in strict comptime
      // comptime {
      //   val m = Map(1 -> "a", 2 -> "b")
      //   m.values.toList.sorted
      // } -> List("a", "b"),

      // TODO: Map.keySet is not supported in strict comptime
      // comptime {
      //   val m = Map(1 -> "a", 2 -> "b")
      //   m.keySet
      // } -> Set(1, 2),

      // TODO: Map.updatedWith is not supported in strict comptime
      // comptime {
      //   val m = Map(1 -> "a")
      //   m.updatedWith(1)(_.map(_ + "!"))
      // } -> Map(1 -> "a!"),

      // TODO: Map.updatedWith is not supported in strict comptime
      // comptime {
      //   val m = Map(1 -> "a")
      //   m.updatedWith(2)(_ => Some("b"))
      // } -> Map(1 -> "a", 2 -> "b"),

      // Option.zip tests moved to OptionSpec.scala

      // TODO: sizeCompare is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3)
      //   xs.sizeCompare(2)
      // } -> 1,

      // TODO: sizeCompare is not supported in strict comptime
      // comptime {
      //   val xs = List(1)
      //   xs.sizeCompare(List(1, 2))
      // } -> -1,

      // TODO: sizeCompare is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2)
      //   xs.sizeCompare(List(1, 2))
      // } -> 0,

      comptime {
        val xs = List(1, 2, 3)
        xs.tail
      } -> List(2, 3),
      comptime {
        val xs = List(1, 2)
        xs.tails.toList
      } -> List(List(1, 2), List(2), List()),
      comptime {
        val xs = List(1, 2)
        xs.inits.toList
      } -> List(List(1, 2), List(1), List()),
      comptime {
        val xs = List(1, 2)
        xs.toIndexedSeq.toVector
      } -> Vector(1, 2),
      comptime {
        val xs = List(1)
        xs match
          case List(a, b, _*) => a + b
          case _              => 0
      } -> 0,
      comptime {
        val xs = List.empty[Int]
        xs match
          case List() => 1
          case _      => 0
      } -> 1,
      comptime {
        val xs = List(1, 2, 3)
        xs match
          case List(_*) => 1
          case _        => 0
      } -> 1,
      comptime {
        val xs = List(1, 2, 3)
        xs match
          case List(rest*) => rest.size
          case _           => 0
      } -> 3,
      comptime {
        val xs = List(1, 2, 3)
        xs match
          case List(rest*) => rest.size
          case _           => 0
      } -> 3,
      comptime {
        val xs = List(1, 2, 3)
        xs match
          case List(a, rest*) => a + rest.size
          case _              => 0
      } -> 3,
      comptime {
        val xs = List(1, 2, 3)
        xs match
          case List(a, rest*) => a + rest.size
          case _              => 0
      } -> 3,
      comptime {
        val xs = List(1, 2, 3, 4)
        xs match
          case List(a, b, rest*) => a + b + rest.size
          case _                 => 0
      } -> 5,
      comptime {
        val xs = List(1, 2, 3, 4)
        xs match
          case List(a, b, rest*) => a + b + rest.size
          case _                 => 0
      } -> 5,
      comptime {
        val xs = List(1, 2)
        xs match
          case List(a, b, rest*) => a + b + rest.size
          case _                 => 0
      } -> 3,
      comptime {
        val xs = List(1)
        xs match
          case List(a, b, rest*) => a + b + rest.size
          case _                 => 0
      } -> 0,
      comptime {
        val xs = Vector(1, 2, 3)
        xs match
          case Vector(a, rest*) => a + rest.size
          case _                => 0
      } -> 3,
      comptime {
        val xs = Vector(1, 2, 3)
        xs match
          case Vector(a, rest*) => a + rest.size
          case _                => 0
      } -> 3,

      // TODO: Seq(...) pattern matching with SeqFactory is not supported in strict comptime
      // comptime {
      //   val xs: Seq[Int] = Seq(1, 2, 3)
      //   xs match
      //     case Seq(a, rest*) => a + rest.size
      //     case _             => 0
      // } -> 3,

      // TODO: Seq(...) pattern matching with SeqFactory is not supported in strict comptime
      // comptime {
      //   val xs: Seq[Int] = Seq(1, 2, 3)
      //   xs match
      //     case Seq(a, rest*) => a + rest.size
      //     case _             => 0
      // } -> 3,

      // Match expressions (case-class unapply patterns)
      comptime {
        val p = Person("abc", 2)
        p match
          case Person(n, a) => n.length + a
      } -> 5,
      comptime {
        val p: Any = Person("abc", 2)
        p match
          case x: Person => x.age
          case _         => 0
      } -> 2,
      comptime {
        val p: Any = Person("abc", 2)
        p match
          case Person(n, a) => n.length + a
          case _            => 0
      } -> 5,
      comptime {
        val p: Any = Person("abc", 2)
        p match
          case Person(n: String, a: Int) => n.length + a
          case _                         => 0
      } -> 5,
      comptime {
        val x: Option[Person] = Some(Person("abc", 2))
        x match
          case Some(Person(n, a)) => n.length + a
          case _                  => 0
      } -> 5,

      // Match expressions (alternatives + outer binding)
      comptime {
        val x = 2
        x match
          case n @ (1 | 2) => n + 1
          case _           => 0
      } -> 3,
      comptime {
        val x = 1
        x match
          case n @ (1 | 2) if n == 2 => 1
          case _                     => 0
      } -> 0,
      comptime {
        val x = 2
        x match
          case n @ (1 | 2) if n == 2 => 1
          case _                     => 0
      } -> 1,
      comptime {
        val x: Any = List(1, 2, 3)
        x match
          case List(1, _*) | Vector(1, _*) => 1
          case _                           => 0
      } -> 1,
      comptime {
        val x: Any = Vector(1, 2, 3)
        x match
          case List(1, _*) | Vector(1, _*) => 1
          case _                           => 0
      } -> 1,

      // Match expressions (nested unapply patterns)
      comptime {
        val x: Option[(Int, Int)] = Some((1, 2))
        x match
          case Some((a, b)) => a + b
          case None         => 0
      } -> 3,

      // TODO: `::` pattern matching is not supported in strict comptime
      // comptime {
      //   val xs = List(1, 2, 3)
      //   xs match
      //     case a :: b :: t => a + b + t.size
      //     case _           => 0
      // } -> 4,

      // Match expressions (unapply patterns: Either / Try)
      comptime {
        val e: Either[String, Int] = Right(1)
        e match
          case Right(n) => n + 1
          case Left(_)  => 0
      } -> 2,
      comptime {
        val e: Either[String, (Int, Int)] = Right((1, 2))
        e match
          case Right((a, b)) => a * 10 + b
          case _             => 0
      } -> 12,
      comptime {
        val t = scala.util.Try(1)
        t match
          case scala.util.Success(v) => v + 1
          case scala.util.Failure(_) => 0
      } -> 2,
      comptime {
        val t = scala.util.Try(1 / 0)
        t match
          case scala.util.Success(_) => 0
          case scala.util.Failure(_) => 1
      } -> 1,
      comptime {
        val x: Option[Any] = Some("abc")
        x match
          case Some(s: String) => s.length
          case _               => 0
      } -> 3,

      // For-comprehensions (desugaring to withFilter/map/flatMap)
      comptime {
        for x <- List(1, 2, 3) yield x * 2
      } -> List(2, 4, 6),

      comptime(List(1, 2, 3).withFilter(x => x > 1).map(x => x * 2)) -> List(4, 6),

      comptime {
        for
          x <- List(1, 2)
          y <- List(10, 20)
        yield x + y
      } -> List(11, 21, 12, 22),

      // For-comprehensions over Option (desugaring to withFilter/map/flatMap)
      comptime {
        for x <- Option(1) yield x + 1
      } -> Some(2),

      comptime {
        for x <- Option(1) if x > 0 yield x + 1
      } -> Some(2),
      comptime(Option(1).withFilter(x => x > 0).map(x => x + 1)) -> Some(2),

      comptime {
        for
          x <- Option(1)
          y <- Option(2)
        yield x + y
      } -> Some(3),
      comptime {
        for
          x <- Option(1)
          y <- Option.empty[Int]
        yield x + y
      } -> None
    )

   */
