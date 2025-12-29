package comptime

import zio.test.*

object ComptimeSpec extends ZIOSpecDefault:
  val spec =
    suite("ComptimeSpec")(
      test("int ops") {
        assertTrue(
          comptime(1 + 2) == 3,
          comptime(5 - 2) == 3,
          comptime(4 * 3) == 12,
          comptime(8 / 2) == 4,
          comptime(8 % 3) == 2,
          comptime(-3) == -3,
          comptime(4 == 4),
          !comptime(4 == 5),
          comptime(4 != 5),
          comptime(2 < 3),
          comptime(2 <= 2),
          comptime(3 > 2),
          comptime(3 >= 3)
        )
      },
      test("long ops") {
        assertTrue(
          comptime(1L + 2L) == 3L,
          comptime(5L - 2L) == 3L,
          comptime(4L * 3L) == 12L,
          comptime(8L / 2L) == 4L,
          comptime(8L % 3L) == 2L,
          comptime(-3L) == -3L,
          comptime(4L == 4L),
          !comptime(4L == 5L),
          comptime(4L != 5L),
          comptime(2L < 3L),
          comptime(2L <= 2L),
          comptime(3L > 2L),
          comptime(3L >= 3L)
        )
      },
      test("double ops") {
        assertTrue(
          comptime(1.0 + 2.0) == 3.0,
          comptime(5.0 - 2.0) == 3.0,
          comptime(4.0 * 3.0) == 12.0,
          comptime(8.0 / 2.0) == 4.0,
          comptime(8.5 % 3.0) == 2.5,
          comptime(-3.0) == -3.0,
          comptime(4.0 == 4.0),
          !comptime(4.0 == 5.0),
          comptime(4.0 != 5.0),
          comptime(2.0 < 3.0),
          comptime(2.0 <= 2.0),
          comptime(3.0 > 2.0),
          comptime(3.0 >= 3.0)
        )
      },
      test("bool ops") {
        assertTrue(
          comptime(!true) == false,
          comptime(!false) == true,
          comptime(true == true),
          comptime(true != false),
          comptime(true & false) == false,
          comptime(true | false) == true,
          comptime(true ^ false) == true,
          comptime(true ^ true) == false,
          comptime(false && (1 / 0 == 0)) == false,
          comptime(true || (1 / 0 == 0)) == true
        )
      },
      test("if/else") {
        assertTrue(
          comptime(if 1 + 1 == 2 then 10 else 20) == 10,
          comptime(if true then 1 else 1 / 0) == 1
        )
      },
      test("block vals") {
        assertTrue(
          comptime {
            val x = 1
            val y = 2
            x + y
          } == 3,
          comptime {
            val base = 10
            if base > 5 then base + 1 else base - 1
          } == 11
        )
      },
      test("option constructors") {
        assertTrue(
          comptime(Option.empty[Int]) == None,
          comptime(Option(1)) == Some(1),
          comptime(Some(2)) == Some(2)
        )
      },
      test("option helpers") {
        assertTrue(
          comptime(Option.when(true)(1)) == Some(1),
          comptime(Option.when(false)(1)) == None,
          comptime(Option.unless(true)(1)) == None,
          comptime(Option.unless(false)(1)) == Some(1),
          comptime(Option(1).getOrElse(2)) == 1,
          comptime(None.getOrElse(2)) == 2
        )
      },
      test("either constructors") {
        assertTrue(
          comptime(Right(1)) == Right(1),
          comptime(Left("err")) == Left("err"),
          comptime(Right(2): Either[String, Int]) == Right(2)
        )
      },
      test("either helpers") {
        assertTrue(
          comptime(Either.cond(true, 1, "err")) == Right(1),
          comptime(Either.cond(false, 1, "err")) == Left("err"),
          comptime(Right(1).getOrElse(2)) == 1,
          comptime(Left("err").getOrElse(2)) == 2
        )
      },
      test("try helpers") {
        assertTrue(
          comptime(scala.util.Try(1).isSuccess),
          comptime(scala.util.Try(1 / 0).isFailure),
          comptime(scala.util.Try(1).getOrElse(2)) == 1,
          comptime(scala.util.Try(1 / 0).getOrElse(2)) == 2
        )
      },
      test("map/flatMap basics") {
        assertTrue(
          comptime(Option(1).map(_ + 1)) == Some(2),
          comptime(Option(1).flatMap(x => Option(x + 1))) == Some(2),
          comptime(Right(1).map(_ + 1)) == Right(2),
          comptime(Right(1).flatMap(x => Right(x + 1))) == Right(2),
          comptime(scala.util.Try(1).map(_ + 1).getOrElse(0)) == 2
        )
      },
      test("more combinators") {
        assertTrue(
          comptime(Option(1).filter(_ > 0)) == Some(1),
          comptime(Option(1).filterNot(_ > 0)) == None,
          comptime((Right(1): Either[String, Int]).swap) == Left(1),
          comptime(scala.util.Try(1 / 0).recover(_ => 5).getOrElse(0)) == 5,
          comptime(Option(1).fold(0)(_ + 1)) == 2,
          comptime((Left("err"): Either[String, Int]).fold(_ => 0, _ + 1)) == 0,
          comptime(scala.util.Try(1).fold(_ => 0, _ + 1)) == 2
        )
      },
      test("list/vector constructors") {
        assertTrue(
          comptime(List(1, 2, 3)) == List(1, 2, 3),
          comptime(List.empty[Int]) == Nil,
          comptime(Vector(1, 2)) == Vector(1, 2),
          comptime(Vector.empty[Int]) == Vector.empty
        )
      },
      test("list/vector ops") {
        assertTrue(
          comptime(List(1, 2, 3).size) == 3,
          comptime(List(1, 2, 3).length) == 3,
          comptime(List(1, 2, 3).isEmpty) == false,
          comptime(List(1, 2, 3).nonEmpty) == true,
          comptime(List(1, 2, 3).head) == 1,
          comptime(List(1, 2, 3).tail) == List(2, 3),
          comptime(List(1, 2).map(_ + 1)) == List(2, 3),
          comptime(List(1, 2).flatMap(x => List(x, x + 1))) == List(1, 2, 2, 3),
          comptime(List(1, 2, 3).filter(_ > 1)) == List(2, 3),
          comptime(List(1, 2, 3).filterNot(_ > 1)) == List(1),
          comptime(Vector(1, 2).map(_ + 1)) == Vector(2, 3),
          comptime(Vector(1, 2, 3).flatMap(x => Vector(x, x + 1))) == Vector(1, 2, 2, 3, 3, 4),
          comptime(Vector(1, 2, 3).filter(_ > 1)) == Vector(2, 3),
          comptime(Vector(1, 2, 3).filterNot(_ > 1)) == Vector(1)
        )
      },
      test("seq helpers") {
        assertTrue(
          comptime(List(1, 2, 3).take(2)) == List(1, 2),
          comptime(Vector(1, 2, 3).drop(1)) == Vector(2, 3),
          comptime(List(1, 2) ++ List(3)) == List(1, 2, 3),
          comptime(Vector(1, 2) ++ Vector(3)) == Vector(1, 2, 3),
          comptime(List(1, 2, 3).mkString) == "123",
          comptime(List(1, 2, 3).mkString(",")) == "1,2,3",
          comptime(List(1, 2, 3).mkString("[", ",", "]")) == "[1,2,3]",
          comptime(List(1, 2, 3).contains(2)),
          comptime(List(1, 2, 3).updated(1, 9)) == List(1, 9, 3),
          comptime(Vector(1, 2, 1).distinct) == Vector(1, 2),
          comptime(List(1, 2, 3).reverse) == List(3, 2, 1)
        )
      },
      test("set/map basics") {
        assertTrue(
          comptime(Set.empty[Int]) == Set.empty,
          comptime(Set(1, 2, 2)) == Set(1, 2),
          comptime(Set(1, 2, 3).contains(2)),
          comptime(Set(1, 2, 3).size) == 3,
          comptime(Set(1, 2, 3).isEmpty) == false,
          comptime(Set(1, 2, 3).nonEmpty) == true,
          comptime(Map.empty[String, Int]) == Map.empty,
          comptime(Map("a" -> 1, "b" -> 2).size) == 2,
          comptime(Map("a" -> 1, "b" -> 2).contains("a")),
          comptime(Map("a" -> 1, "b" -> 2).get("a")) == Some(1),
          comptime(Map("a" -> 1, "b" -> 2).apply("b")) == 2,
          comptime(Map("a" -> 1).getOrElse("a", 1 / 0)) == 1,
          comptime(Map("a" -> 1).getOrElse("b", 2)) == 2,
          comptime(Set(1, 2) ++ Set(2, 3)) == Set(1, 2, 3),
          comptime(Map("a" -> 1) ++ Map("b" -> 2)) == Map("a" -> 1, "b" -> 2),
          comptime(Map("a" -> 1).updated("b", 2)) == Map("a" -> 1, "b" -> 2),
          comptime(Map("a" -> 1).removed("a")) == Map.empty[String, Int],
          comptime(Map("a" -> 1, "b" -> 2).keys.toSet) == Set("a", "b"),
          comptime(Map("a" -> 1, "b" -> 2).values.toSet) == Set(1, 2),
          comptime(Map("a" -> 1, "b" -> 2).keySet) == Set("a", "b")
        )
      },
      test("match basics") {
        assertTrue(
          comptime {
            val x = 1
            x match
              case 1 => 2
              case _ => 0
          } == 2,
          comptime {
            val x: Any = 2
            x match
              case i: Int => i + 1
              case _      => 0
          } == 3,
          comptime {
            val x = 2
            x match
              case v if v > 1 => v + 1
              case _          => 0
          } == 3,
          comptime {
            val x = 0
            x match
              case v if v > 1 => v + 1
              case _          => 0
          } == 0,
          comptime {
            val x = (1, 2)
            x match
              case (a, b) => a + b
          } == 3,
          comptime {
            val x = 2
            x match
              case 1 | 2 => 1
              case _     => 0
          } == 1
        )
      },
      test("match unapply") {
        assertTrue(
          comptime {
            val x: Option[Int] = Some(1)
            x match
              case Some(v) => v + 1
              case _       => 0
          } == 2,
          comptime {
            val x: Option[Int] = None
            x match
              case None => 1
              case _    => 0
          } == 1,
          comptime {
            val x: Any = Some(1)
            x match
              case Some(v: Int) => v + 1
              case _            => 0
          } == 2,
          comptime {
            val x: Either[String, Int] = Right(2)
            x match
              case Right(v) => v
              case _        => 0
          } == 2,
          comptime {
            val xs = List(1, 2, 3)
            xs match
              case List(a, rest*) => a + rest.size
              case _              => 0
          } == 3,
          comptime {
            val xs = List(1, 2)
            xs match
              case List(_*) => 1
              case _        => 0
          } == 1,
          comptime {
            val xs = List(1, 2)
            xs match
              case List(a, b) => a + b
              case _          => 0
          } == 3,
          comptime {
            val xs = List.empty[Int]
            xs match
              case List() => 1
              case _      => 0
          } == 1,
          comptime {
            val xs = Array(1, 2)
            xs match
              case Array(a, b) => a + b
              case _           => 0
          } == 3,
          comptime {
            val xs = Array(1, 2, 3)
            xs match
              case Array(a, rest*) => a + rest.size
              case _               => 0
          } == 3,
          comptime {
            val xs = Array(1, 2)
            xs match
              case Array(_*) => 1
              case _         => 0
          } == 1,
          comptime {
            val xs = Array.empty[Int]
            xs match
              case Array() => 1
              case _       => 0
          } == 1,
          // :: pattern matching
          comptime {
            val xs = List(1, 2, 3)
            xs match
              case h :: t => h + t.size
              case _      => 0
          } == 3,
          comptime {
            val xs = List(2, 3)
            xs match
              case h :: t if h > 1 => h + t.size
              case _               => 0
          } == 3,
          comptime {
            val xs = List.empty[Int]
            xs match
              case Nil => 1
              case _   => 0
          } == 1,
          comptime {
            val xs: Seq[Int] = Vector(1, 2)
            xs match
              case Seq(a, b) => a + b
              case _         => 0
          } == 3,
          comptime {
            val xs = Vector(1, 2, 3)
            xs match
              case Vector(a, b, _*) => a + b
              case _                => 0
          } == 3,
          comptime {
            val x: Any = List(1, 2, 3)
            x match
              case List(_, _*) | Vector(_, _*) => 1
              case _                           => 0
          } == 1,
          comptime {
            val x = 2
            x match
              case y @ (1 | 2) => y
              case _           => 0
          } == 2,
          comptime {
            val x = (1, 2, 3, 4)
            x match
              case (a, b, c, d) => a + b + c + d
          } == 10,
          comptime {
            val x: Option[(Int, Int, Int, Int)] = Some((1, 2, 3, 4))
            x match
              case Some((a, b, c, d)) => a + b + c + d
              case _                  => 0
          } == 10,
          comptime {
            val x = (1, 2, 3, 4, 5)
            x match
              case (a, b, c, d, e) => a + b + c + d + e
          } == 15,
          comptime {
            val x: Either[String, (Int, Int)] = Right((1, 2))
            x match
              case Right((a, b)) => a + b
              case _             => 0
          } == 3,
          comptime {
            val p = Person("a", 2)
            p match
              case Person(name, age) => age + name.length
              case _                 => 0
          } == 3,
          comptime {
            val p: Any = Person("b", 4)
            p match
              case Person(n: String, a: Int) => a + n.length
              case _                         => 0
          } == 5,
          // TODO: Singleton object reference not supported in strict comptime
          // comptime {
          //   val x: Any = Singleton
          //   x match
          //     case Singleton => 1
          //     case _         => 0
          // } == 1,
          // comptime {
          //   val x: Any = Singleton
          //   x match
          //     case Singleton => 1
          //     case _         => 0
          // } == 1,
          comptime {
            val xs = List(Some(1), Some(2))
            xs match
              case List(Some(a), _*) => a
              case _                 => 0
          } == 1,
          comptime {
            val xs: Seq[Int] = Seq.empty[Int]
            xs match
              case Seq() => 1
              case _     => 0
          } == 1,
          comptime {
            val xs = List(Some(1), Some(2))
            xs match
              case List(Some(a), _*) => a
              case _                 => 0
          } == 1,
          comptime {
            val xs = Vector(Some(1), None)
            xs match
              case Vector(Some(a), _*) => a
              case _                   => 0
          } == 1,
          comptime {
            val xs = List(1, 2, 3)
            xs match
              case List(1 | 2, _*) => 1
              case _               => 0
          } == 1,
          comptime {
            val t = scala.util.Try(1)
            t match
              case scala.util.Success(v) => v
              case _                     => 0
          } == 1,
          comptime {
            val t = scala.util.Try(1 / 0)
            t match
              case scala.util.Failure(_) => 1
              case _                     => 0
          } == 1,
          comptime {
            val xs = List(1, 2, 3)
            xs match
              case List(a, b, _*) => a + b
              case _              => 0
          } == 3,
          comptime {
            val xs = List(1, 2, 3)
            xs match
              case List(a, rest*) => a + rest.size
              case _              => 0
          } == 3,
          comptime {
            val xs: Any = Array(1, 2)
            xs match
              case Array(a: Int, b: Int) => a + b
              case _                     => 0
          } == 3,
          comptime {
            val x = 2
            x match
              case y @ (1 | 2) => y + 1
              case _           => 0
          } == 3
          // TODO: Box[Int] pattern matching has ClassCastException
          // comptime {
          //   val b: Box[Int] = Box(3)
          //   b match
          //     case Box(v) => v + 1
          //     case _      => 0
          // } == 4
        )
      },
      test("varargs case class patterns") {
        assertTrue(
          comptime {
            val x: VarBox[Int] = VarBox(1, 2, 3)
            x.rest.size
          } == 2,
          comptime {
            val x: VarBox[Int] = VarBox(1, 2, 3)
            x match
              case VarBox(a, b, c) => a + b + c
              case _               => 0
          } == 6,
          comptime {
            val x: VarBox[Int] = VarBox(1)
            x match
              case VarBox(a) => a
              case _         => 0
          } == 1,
          comptime {
            val x: VarBox[Int] = VarBox(1, 2)
            x match
              case VarBox(a) => a
              case _         => 0
          } == 0,
          comptime {
            val x: VarBox[Int] = VarBox(1, 2, 3, 4)
            x match
              case VarBox(a, rest*) => a + rest.size
              case _                => 0
          } == 4,
          comptime {
            val x: VarBox[Int] = VarBox(1, 2, 3)
            x match
              case VarBox(a, b, _*) => a + b
              case _                => 0
          } == 3,
          comptime {
            val x: VarBox[Int] = VarBox(1, 2, 3)
            x match
              case VarBox(_, _*) => 1
              case _             => 0
          } == 1,
          comptime {
            val x: Any = VarBox(1, 2)
            x match
              case VarBox(_, _*) => 1
              case _             => 0
          } == 1,
          comptime {
            val x: Any = VarBox(1, 2)
            x match
              case VarBox(a: Int, _*) => a
              case _                  => 0
          } == 1,
          comptime {
            val x: Any = VarBox(1, 2, 3, 4)
            x match
              case VarBox(a: Int, b: Int, rest*) => a + b + rest.size
              case _                             => 0
          } == 5,
          comptime {
            val x: Any = VarBox(1, 2, 3, 4)
            x match
              case VarBox(a: Int, rest*) => a + rest.size
              case _                     => 0
          } == 4,
          comptime {
            val x: Any = VarBox(1, 2, 3, 4)
            x match
              case VarBox(rest*) => rest.size
              case _             => 0
          } == 3,
          comptime {
            val x: Any = 1
            x match
              case VarBox(a: Int, rest*) => a + rest.size
              case _                     => 0
          } == 0,
          comptime {
            val x: Any = 1
            x match
              case VarBox(_, _*) => 1
              case _             => 0
          } == 0,
          comptime {
            val x: Any = 1
            x match
              case VarBox(a: Int, _*) => a
              case _                  => 0
          } == 0,
          comptime {
            val x: Any = VarBox(1, 2)
            x match
              case VarBox(a: Int, b: Int) => a + b
              case _                      => 0
          } == 3
        )
      },
      // TODO: Custom extractors from TestExtractors not supported in strict comptime
      // The following extractors are unsupported: Two, Parts, PairBox, Triple, PersonBox,
      // IsEven, Even, Half, Split, Always, Overloaded, ArrParts, TrioRest
      // Also: mkString on iterators (Regex varargs pattern)
      // test("custom extractors") {
      //   assertTrue(
      //     comptime {
      //       val x = 4
      //       x match
      //         case TestExtractors.Even() => 1
      //         case _                     => 0
      //     } == 1,
      //     comptime {
      //       val x = 3
      //       x match
      //         case TestExtractors.Even() => 1
      //         case _                     => 0
      //     } == 0,
      //     comptime {
      //       val x = 4
      //       x match
      //         case TestExtractors.Half(n) => n
      //         case _                      => 0
      //     } == 2,
      //     comptime {
      //       val x: Any = "x"
      //       x match
      //         case TestExtractors.Half(n) => n
      //         case _                      => 0
      //     } == 0,
      //     comptime {
      //       val s = "a:b"
      //       s match
      //         case TestExtractors.Split(a, b) => a + b
      //         case _                          => ""
      //     } == "ab",
      //     comptime {
      //       val s = "ab"
      //       s match
      //         case TestExtractors.Split(a, b) => a + b
      //         case _                          => ""
      //     } == "",
      //     comptime {
      //       val s: Any = "a:b"
      //       s match
      //         case TestExtractors.Split(a, b) => a + b
      //         case _                          => ""
      //     } == "ab",
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.Always(()) => 1
      //         case _                         => 0
      //     } == 1,
      //     comptime {
      //       val x = -1
      //       x match
      //         case TestExtractors.Always(()) => 1
      //         case _                         => 0
      //     } == 0,
      //     comptime {
      //       val r = "([a-z]+)(\\d+)".r
      //       "abc123" match
      //         case r(word, num) => word + num
      //         case _            => ""
      //     } == "abc123",
      //     comptime {
      //       val r = "([a-z]+)(\\d+)".r
      //       "abc" match
      //         case r(word, num) => word + num
      //         case _            => "no"
      //     } == "no",
      //     comptime {
      //       val r = "([a-z]+)(\\d+)([A-Z]+)".r
      //       "abc123XYZ" match
      //         case r(word, rest*) => word + rest.mkString
      //         case _              => ""
      //     } == "abc123XYZ",
      //     comptime {
      //       val r = "([a-z]+)(\\d+)".r
      //       "abc123" match
      //         case r(_*) => 1
      //         case _     => 0
      //     } == 1,
      //     comptime {
      //       val x = 3
      //       x match
      //         case TestExtractors.Overloaded(n) => n + 1
      //         case _                            => 0
      //     } == 4,
      //     comptime {
      //       val x = "abcd"
      //       x match
      //         case TestExtractors.Overloaded(n) => n
      //         case _                            => 0
      //     } == 4,
      //     comptime {
      //       val x = 4
      //       x match
      //         case TestExtractors.IsEven() => 1
      //         case _                       => 0
      //     } == 1,
      //     comptime {
      //       val x = 3
      //       x match
      //         case TestExtractors.IsEven() => 1
      //         case _                       => 0
      //     } == 0,
      //     comptime {
      //       val x: Any = 4
      //       x match
      //         case TestExtractors.IsEven() => 1
      //         case _                       => 0
      //     } == 1,
      //     comptime {
      //       val x = 5
      //       x match
      //         case TestExtractors.PersonBox(name, age) => name.length + age
      //         case _                                   => 0
      //     } == 6,
      //     comptime {
      //       val x: Any = 2
      //       x match
      //         case TestExtractors.PersonBox(name, age) => name.length + age
      //         case _                                   => 0
      //     } == 3,
      //     comptime {
      //       val x = 2
      //       x match
      //         case TestExtractors.PersonBox(name: String, age: Int) => name.length + age
      //         case _                                                => 0
      //     } == 3,
      //     comptime {
      //       val x: Any = 2
      //       x match
      //         case TestExtractors.PersonBox(name: String, age: Int) => name.length + age
      //         case _                                                => 0
      //     } == 3,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.Two(a, b) => a + b
      //         case _                        => 0
      //     } == 3,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.Two(_*) => 1
      //         case _                      => 0
      //     } == 1,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.Two(a: Int, b: Int) => a + b
      //         case _                                  => 0
      //     } == 3,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.Two(a: Int, _*) => a
      //         case _                              => 0
      //     } == 1,
      //     comptime {
      //       val x: Any = 1
      //       x match
      //         case TestExtractors.Two(a: Int, _*) => a
      //         case _                              => 0
      //     } == 1,
      //     comptime {
      //       val x: Any = 1
      //       x match
      //         case TestExtractors.Two(a, b) => a + b
      //         case _                        => 0
      //     } == 3,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.Two(rest*) => rest.head + rest.last
      //         case _                         => 0
      //     } == 3,
      //     comptime {
      //       val x: Any = 1
      //       x match
      //         case TestExtractors.Two(rest*) => rest.head + rest.last
      //         case _                         => 0
      //     } == 3,
      //     comptime {
      //       val x: Any = -1
      //       x match
      //         case TestExtractors.Two(_*) => 1
      //         case _                      => 0
      //     } == 0,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.ArrParts(a, rest*) => a + rest.size
      //         case _                                 => 0
      //     } == 3,
      //     comptime {
      //       val x: Any = 1
      //       x match
      //         case TestExtractors.ArrParts(a: Int, rest*) => a + rest.size
      //         case _                                      => 0
      //     } == 3,
      //     comptime {
      //       val x: Any = 1
      //       x match
      //         case TestExtractors.ArrParts(a: Int, b: Int, _*) => a + b
      //         case _                                           => 0
      //     } == 3,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.ArrParts(_, _*) => 1
      //         case _                              => 0
      //     } == 1,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.TrioRest(a, b, rest*) => a + b + rest.size
      //         case _                                    => 0
      //     } == 5,
      //     comptime {
      //       val x: Any = 1
      //       x match
      //         case TestExtractors.TrioRest(a: Int, b: Int, rest*) => a + b + rest.size
      //         case _                                              => 0
      //     } == 5,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.TrioRest(a, b, c, d) => a + b + c + d
      //         case _                                   => 0
      //     } == 10,
      //     comptime {
      //       val x: Any = -1
      //       x match
      //         case TestExtractors.ArrParts(_, _*) => 1
      //         case _                              => 0
      //     } == 0,
      //     comptime {
      //       val x = 1
      //       x match
      //         case TestExtractors.Triple(a, b, c) => a + b + c
      //         case _                              => 0
      //     } == 6,
      //     comptime {
      //       val s = "a-b-c"
      //       s match
      //         case TestExtractors.Parts(_, _*) => 1
      //         case _                           => 0
      //     } == 1,
      //     comptime {
      //       val s = "a-b-c"
      //       s match
      //         case TestExtractors.Parts(head, second, rest*) => head + second + rest.mkString
      //         case _                                         => ""
      //     } == "abc",
      //     comptime {
      //       val s = "a-b"
      //       s match
      //         case TestExtractors.Parts(head, tail) => head + tail
      //         case _                                => ""
      //     } == "ab",
      //     comptime {
      //       val s = "a"
      //       s match
      //         case TestExtractors.Parts(head, rest*) => head + rest.mkString
      //         case _                                 => ""
      //     } == "a",
      //     comptime {
      //       val s: Any = 1
      //       s match
      //         case TestExtractors.Parts(_, _*) => 1
      //         case _                           => 0
      //     } == 0,
      //     comptime {
      //       val s: Any = "a-b-c"
      //       s match
      //         case TestExtractors.Parts(head, rest*) => head + rest.mkString
      //         case _                                 => ""
      //     } == "abc",
      //     comptime {
      //       val s: Any = "a-b"
      //       s match
      //         case TestExtractors.Parts(head, tail) => head + tail
      //         case _                                => ""
      //     } == "ab",
      //     comptime {
      //       val s = "a-b-c-d"
      //       s match
      //         case TestExtractors.PairBox(p, rest*) =>
      //           p.a + p.b + rest.mkString
      //         case _ =>
      //           ""
      //     } == "abcd",
      //     comptime {
      //       val s: Any = "a-b-c"
      //       s match
      //         case TestExtractors.PairBox(p, rest*) =>
      //           p.a + p.b + rest.mkString
      //         case _ =>
      //           ""
      //     } == "abc",
      //     comptime {
      //       val s: Any = "a"
      //       s match
      //         case TestExtractors.PairBox(_, _*) => 1
      //         case _                             => 0
      //     } == 0,
      //     comptime {
      //       val s = "a-b-c"
      //       s match
      //         case TestExtractors.PairBox(p, rest*) =>
      //           p.a + p.b + rest.mkString
      //         case _ =>
      //           ""
      //     } == "abc",
      //     comptime {
      //       val s = "a-b-c"
      //       s match
      //         case TestExtractors.PairBox(TestExtractors.PairBox.Pair(x, y), rest*) =>
      //           x + y + rest.mkString
      //         case _ =>
      //           ""
      //     } == "abc",
      //     comptime {
      //       val s: Any = "a-b-c"
      //       s match
      //         case TestExtractors.PairBox(TestExtractors.PairBox.Pair(x, y), rest*) =>
      //           x + y + rest.mkString
      //         case _ =>
      //           ""
      //     } == "abc",
      //     comptime {
      //       val x: Any = 1
      //       x match
      //         case TestExtractors.Triple(a: Int, b: Int, c: Int) => a + b + c
      //         case _                                             => 0
      //     } == 6,
      //     comptime {
      //       val x = 2
      //       x match
      //         case TestExtractors.Triple(TestExtractors.Half(a), b, c) => a + b + c
      //         case _                                                   => 0
      //     } == 8,
      //     comptime {
      //       val x = 3
      //       x match
      //         case TestExtractors.Triple(a, TestExtractors.Half(b), c) => a + b + c
      //         case _                                                   => 0
      //     } == 10,
      //     comptime {
      //       val x: Option[Int] = Some(1)
      //       x match
      //         case Some(TestExtractors.Triple(a, b, c)) => a + b + c
      //         case _                                    => 0
      //     } == 6,
      //     comptime {
      //       val x: Any = -1
      //       x match
      //         case TestExtractors.Triple(a, b, c) => a + b + c
      //         case _                              => 0
      //     } == 0
      //   )
      // },
      test("string ops") {
        assertTrue(
          comptime("foo" + "bar") == "foobar",
          comptime("foo" + 1) == "foo1",
          comptime("Foo".toUpperCase) == "FOO",
          comptime("Foo".toLowerCase) == "foo",
          comptime("  a  ".trim) == "a",
          comptime("".isEmpty),
          comptime("abc".contains("b")),
          comptime("abc".startsWith("a")),
          comptime("abc".endsWith("c")),
          comptime("abcdef".indexOf("cd")) == 2,
          comptime("abcdef".indexOf("cd", 3)) == -1,
          comptime("abcdef".lastIndexOf("cd")) == 2,
          comptime("abcdef".lastIndexOf("cd", 1)) == -1,
          comptime("abcdef" (2)) == 'c',
          comptime("abcdef".charAt(2)) == 'c',
          comptime("abcdef".slice(1, 4)) == "bcd",
          comptime("abcdef".substring(2)) == "cdef",
          comptime("abcdef".substring(1, 4)) == "bcd",
          comptime("abcdef".take(3)) == "abc",
          comptime("abcdef".drop(3)) == "def",
          comptime("abcdef".length) == 6,
          comptime("".nonEmpty) == false,
          comptime("abcdef".startsWith("cd", 2))
        )
      }
    )
