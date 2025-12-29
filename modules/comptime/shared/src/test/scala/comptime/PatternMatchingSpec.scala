package comptime

import zio.test.*

object PatternMatchingSpec extends ZIOSpecDefault:
  case class Box[A](value: A)
  case class VarBox[A](value: A, rest: A*)

  val spec =
    suite("PatternMatchingSpec (comptime)")(
      suite("values")(
        evalTests.map { case (actual, expected) =>
          test(s"comptime($actual) == $expected") {
            assertTrue(actual == expected)
          }
        }
      )
    )

  lazy val evalTests = List(
    // Match expressions (unapplySeq patterns: Array(a, b))
    comptime {
      val xs = Array(1, 2)
      xs match
        case Array(a, b) => a + b
        case _           => 0
    } -> 3,
    comptime {
      val xs = Array(1, 2, 3)
      xs match
        case Array(a, rest*) => a + rest.size
        case _               => 0
    } -> 3,
    comptime {
      val xs = Array.empty[Int]
      xs match
        case Array() => 1
        case _       => 0
    } -> 1,
    comptime {
      val xs = Array(1, 2, 3)
      xs match
        case Array(_*) => 1
        case _         => 0
    } -> 1,
    comptime {
      val x: Any = Array(1, 2)
      x match
        case Array(a: Int, b: Int) => a + b
        case _                     => 0
    } -> 3,
    // TODO: Array with mkString needs ClassTag.apply support
    // comptime {
    //   val xs = Array("a", "b", "c")
    //   xs match
    //     case Array(a, rest*) => a + rest.mkString
    //     case _               => ""
    // } -> "abc",

    // Match expressions (generic case-class unapply patterns)
    comptime {
      val x: Box[Int] = Box(1)
      x match
        case Box(v) => v + 1
    } -> 2,
    comptime {
      val x: Any = Box(1)
      x match
        case Box(v: Int) => v + 1
        case _           => 0
    } -> 2,
    comptime {
      val x: Option[Box[Int]] = Some(Box(1))
      x match
        case Some(Box(v)) => v + 1
        case _            => 0
    } -> 2,
    comptime {
      val x = Box(value = 2)
      x.value
    } -> 2,

    // Varargs case-class apply + unapplySeq patterns
    comptime {
      val x: VarBox[Int] = VarBox(1, 2, 3)
      x.rest.size
    } -> 2,
    comptime {
      val x: VarBox[Int] = VarBox(1, 2, 3)
      x match
        case VarBox(a, b, c) => a + b + c
        case _               => 0
    } -> 6,
    comptime {
      val x: VarBox[Int] = VarBox(1)
      x match
        case VarBox(a) => a
        case _         => 0
    } -> 1,
    comptime {
      val x: VarBox[Int] = VarBox(1, 2)
      x match
        case VarBox(a) => a
        case _         => 0
    } -> 0,
    comptime {
      val x: VarBox[Int] = VarBox(1, 2, 3, 4)
      x match
        case VarBox(a, rest*) => a + rest.size
        case _                => 0
    } -> 4,
    comptime {
      val x: VarBox[Int] = VarBox(1, 2, 3, 4)
      x match
        case VarBox(a, rest*) => a + rest.size
        case _                => 0
    } -> 4,
    comptime {
      val x: VarBox[Int] = VarBox(1, 2, 3)
      x match
        case VarBox(a, b, _*) => a + b
        case _                => 0
    } -> 3,
    comptime {
      val x: VarBox[Int] = VarBox(1, 2, 3)
      x match
        case VarBox(_*) => 1
        case _          => 0
    } -> 1,
    comptime {
      val x: Any = VarBox(1, 2)
      x match
        case VarBox(_*) => 1
        case _          => 0
    } -> 1,
    comptime {
      val x: Any = VarBox(1, 2)
      x match
        case VarBox(a: Int, _*) => a
        case _                  => 0
    } -> 1,
    comptime {
      val x: Any = VarBox(1, 2, 3, 4)
      x match
        case VarBox(a: Int, b: Int, rest*) => a + b + rest.size
        case _                             => 0
    } -> 5,
    comptime {
      val x: Any = VarBox(1, 2, 3, 4)
      x match
        case VarBox(a: Int, rest*) => a + rest.size
        case _                     => 0
    } -> 4,
    comptime {
      val x: Any = VarBox(1, 2, 3, 4)
      x match
        case VarBox(rest*) => rest.size
        case _             => 0
    } -> 3,
    comptime {
      val x: Any = 1
      x match
        case VarBox(a: Int, rest*) => a + rest.size
        case _                     => 0
    } -> 0,
    comptime {
      val x: Any = 1
      x match
        case VarBox(_*) => 1
        case _          => 0
    } -> 0,
    comptime {
      val x: Any = 1
      x match
        case VarBox(a: Int, _*) => a
        case _                  => 0
    } -> 0,
    comptime {
      val x: Any = VarBox(1, 2)
      x match
        case VarBox(a: Int, b: Int) => a + b
        case _                      => 0
    } -> 3,
    comptime {
      val xs = List(1, 2, 3)
      xs match
        case List(a, b, _*) => a + b
        case _              => 0
    } -> 3,
    comptime {
      val xs: Any = List(1, 2, 3)
      xs match
        case List(_*) => 1
        case _        => 0
    } -> 1,
    comptime {
      val xs = Vector(1, 2, 3)
      xs match
        case Vector(a, b, _*) => a + b
        case _                => 0
    } -> 3,
    comptime {
      val xs: Any = Vector(1, 2, 3)
      xs match
        case Vector(_*) => 1
        case _          => 0
    } -> 1,
    comptime {
      val xs: Seq[Int] = Vector(1, 2)
      xs match
        case Seq(a, b) => a + b
        case _         => 0
    } -> 3,
    comptime {
      val x = List(1, 2, 3)
      x match
        case List(1 | 2, _*) => 1
        case _               => 0
    } -> 1,
    comptime {
      val x = List(3, 2, 1)
      x match
        case List(1 | 2, _*) => 1
        case _               => 0
    } -> 0,
    comptime {
      val x = 1
      x match
        case y @ (1 | 2) => y
        case _           => 0
    } -> 1,
    comptime {
      val x = 2
      x match
        case y @ (1 | 2) => y
        case _           => 0
    } -> 2,
    comptime {
      val x: Any = List(1, 2, 3)
      x match
        case a @ (List(_, _*) | Vector(_, _*)) => a.asInstanceOf[Seq[Int]].head
        case _                                 => 0
    } -> 1,
    comptime {
      val x: Any = Vector(4, 5)
      x match
        case a @ (List(_, _*) | Vector(_, _*)) => a.asInstanceOf[Seq[Int]].head
        case _                                 => 0
    } -> 4,
    // TODO: Tuple4.apply not supported in strict comptime
    // comptime {
    //   val x = (1, 2, 3, 4)
    //   x match
    //     case (a, b, c, d) => a + b + c + d
    // } -> 10,
    // comptime {
    //   val x: Option[(Int, Int, Int, Int)] = Some((1, 2, 3, 4))
    //   x match
    //     case Some((a, b, c, d)) => a + b + c + d
    //     case _                  => 0
    // } -> 10,

    // Generic extractors (unapply / unapplySeq)
    // TODO: TestExtractors.Even not supported in strict comptime
    // comptime {
    //   val x = 4
    //   x match
    //     case TestExtractors.Even() => 1
    //     case _                     => 0
    // } -> 1,
    // comptime {
    //   val x = 3
    //   x match
    //     case TestExtractors.Even() => 1
    //     case _                     => 0
    // } -> 0,
    // TODO: TestExtractors.Half not supported in strict comptime
    // comptime {
    //   val x = 4
    //   x match
    //     case TestExtractors.Half(n) => n
    //     case _                      => 0
    // } -> 2,
    // comptime {
    //   val x: Any = "x"
    //   x match
    //     case TestExtractors.Half(n) => n
    //     case _                      => 0
    // } -> 0,
    // TODO: TestExtractors.Split not supported in strict comptime
    // comptime {
    //   val s = "a:b"
    //   s match
    //     case TestExtractors.Split(a, b) => a + b
    //     case _                          => ""
    // } -> "ab",
    // comptime {
    //   val s = "ab"
    //   s match
    //     case TestExtractors.Split(a, b) => a + b
    //     case _                          => ""
    // } -> "",
    // comptime {
    //   val s: Any = "a:b"
    //   s match
    //     case TestExtractors.Split(a, b) => a + b
    //     case _                          => ""
    // } -> "ab",
    // TODO: TestExtractors.Always not supported in strict comptime
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.Always(()) => 1
    //     case _                         => 0
    // } -> 1,
    // comptime {
    //   val x = -1
    //   x match
    //     case TestExtractors.Always(()) => 1
    //     case _                         => 0
    // } -> 0,
    comptime {
      val r = "([a-z]+)(\\d+)".r
      "abc123" match
        case r(word, num) => word + num
        case _            => ""
    } -> "abc123",
    comptime {
      val r = "([a-z]+)(\\d+)".r
      "abc" match
        case r(word, num) => word + num
        case _            => "no"
    } -> "no",
    comptime {
      val r = "([a-z]+)(\\d+)([A-Z]+)".r
      "abc123XYZ" match
        case r(word, rest*) => word + rest.mkString
        case _              => ""
    } -> "abc123XYZ",
    comptime {
      val r = "([a-z]+)(\\d+)".r
      "abc123" match
        case r(_*) => 1
        case _     => 0
    } -> 1
    // TODO: TestExtractors.Parts not supported in strict comptime
    // comptime {
    //   val s = "a-b-c"
    //   s match
    //     case TestExtractors.Parts(_, _*) => 1
    //     case _                           => 0
    // } -> 1,
    // comptime {
    //   val s = "a-b-c"
    //   s match
    //     case TestExtractors.Parts(head, second, rest*) => head + second + rest.mkString
    //     case _                                         => ""
    // } -> "abc",
    // comptime {
    //   val s = "a-b"
    //   s match
    //     case TestExtractors.Parts(head, tail) => head + tail
    //     case _                                => ""
    // } -> "ab",
    // comptime {
    //   val s = "a"
    //   s match
    //     case TestExtractors.Parts(head, rest*) => head + rest.mkString
    //     case _                                 => ""
    // } -> "a",
    // comptime {
    //   val s: Any = 1
    //   s match
    //     case TestExtractors.Parts(_, _*) => 1
    //     case _                           => 0
    // } -> 0,
    // TODO: TestExtractors.Overloaded not supported in strict comptime
    // comptime {
    //   val x = 3
    //   x match
    //     case TestExtractors.Overloaded(n) => n + 1
    //     case _                            => 0
    // } -> 4,
    // comptime {
    //   val x = "abcd"
    //   x match
    //     case TestExtractors.Overloaded(n) => n
    //     case _                            => 0
    // } -> 4,
    // TODO: TestExtractors.IsEven not supported in strict comptime
    // comptime {
    //   val x = 4
    //   x match
    //     case TestExtractors.IsEven() => 1
    //     case _                       => 0
    // } -> 1,
    // comptime {
    //   val x = 3
    //   x match
    //     case TestExtractors.IsEven() => 1
    //     case _                       => 0
    // } -> 0,
    // comptime {
    //   val x: Any = 4
    //   x match
    //     case TestExtractors.IsEven() => 1
    //     case _                       => 0
    // } -> 1,
    // TODO: TestExtractors.PersonBox not supported in strict comptime
    // comptime {
    //   val x = 5
    //   x match
    //     case TestExtractors.PersonBox(name, age) => name.length + age
    //     case _                                   => 0
    // } -> 6,
    // comptime {
    //   val x: Any = 2
    //   x match
    //     case TestExtractors.PersonBox(name, age) => name.length + age
    //     case _                                   => 0
    // } -> 3,
    // comptime {
    //   val x = 2
    //   x match
    //     case TestExtractors.PersonBox(name: String, age: Int) => name.length + age
    //     case _                                                => 0
    // } -> 3,
    // comptime {
    //   val x: Any = 2
    //   x match
    //     case TestExtractors.PersonBox(name: String, age: Int) => name.length + age
    //     case _                                                => 0
    // } -> 3,
    // TODO: TestExtractors.Two not supported in strict comptime
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.Two(a, b) => a + b
    //     case _                        => 0
    // } -> 3,
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.Two(_*) => 1
    //     case _                      => 0
    // } -> 1,
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.Two(a: Int, b: Int) => a + b
    //     case _                                  => 0
    // } -> 3,
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.Two(a: Int, _*) => a
    //     case _                              => 0
    // } -> 1,
    // comptime {
    //   val x: Any = 1
    //   x match
    //     case TestExtractors.Two(a: Int, _*) => a
    //     case _                              => 0
    // } -> 1,
    // comptime {
    //   val x: Any = 1
    //   x match
    //     case TestExtractors.Two(a, b) => a + b
    //     case _                        => 0
    // } -> 3,
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.Two(rest*) => rest.head + rest.last
    //     case _                         => 0
    // } -> 3,
    // comptime {
    //   val x: Any = 1
    //   x match
    //     case TestExtractors.Two(rest*) => rest.head + rest.last
    //     case _                         => 0
    // } -> 3,
    // comptime {
    //   val x: Any = -1
    //   x match
    //     case TestExtractors.Two(_*) => 1
    //     case _                      => 0
    // } -> 0,
    // TODO: TestExtractors.ArrParts not supported in strict comptime
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.ArrParts(a, rest*) => a + rest.size
    //     case _                                 => 0
    // } -> 3,
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.ArrParts(a, rest*) => a + rest.size
    //     case _                                 => 0
    // } -> 3,
    // comptime {
    //   val x: Any = 1
    //   x match
    //     case TestExtractors.ArrParts(a: Int, rest*) => a + rest.size
    //     case _                                      => 0
    // } -> 3,
    // comptime {
    //   val x: Any = 1
    //   x match
    //     case TestExtractors.ArrParts(a: Int, b: Int, _*) => a + b
    //     case _                                           => 0
    // } -> 3,
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.ArrParts(_, _*) => 1
    //     case _                              => 0
    // } -> 1,
    // TODO: TestExtractors.TrioRest not supported in strict comptime
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.TrioRest(a, b, rest*) => a + b + rest.size
    //     case _                                    => 0
    // } -> 5,
    // comptime {
    //   val x: Any = 1
    //   x match
    //     case TestExtractors.TrioRest(a: Int, b: Int, rest*) => a + b + rest.size
    //     case _                                              => 0
    // } -> 5,
    // comptime {
    //   val x: Any = 1
    //   x match
    //     case TestExtractors.TrioRest(a: Int, b: Int, rest*) => a + b + rest.size
    //     case _                                              => 0
    // } -> 5,
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.TrioRest(a, b, c, d) => a + b + c + d
    //     case _                                   => 0
    // } -> 10,
    // comptime {
    //   val x: Any = -1
    //   x match
    //     case TestExtractors.ArrParts(_, _*) => 1
    //     case _                              => 0
    // } -> 0,
    // TODO: TestExtractors.Triple not supported in strict comptime
    // comptime {
    //   val x = 1
    //   x match
    //     case TestExtractors.Triple(a, b, c) => a + b + c
    //     case _                              => 0
    // } -> 6,
    // comptime {
    //   val x: Any = 1
    //   x match
    //     case TestExtractors.Triple(a: Int, b: Int, c: Int) => a + b + c
    //     case _                                             => 0
    // } -> 6,
    // comptime {
    //   val x = 2
    //   x match
    //     case TestExtractors.Triple(TestExtractors.Half(a), b, c) => a + b + c
    //     case _                                                   => 0
    // } -> 8,
    // comptime {
    //   val x = 3
    //   x match
    //     case TestExtractors.Triple(a, TestExtractors.Half(b), c) => a + b + c
    //     case _                                                   => 0
    // } -> 10,
    // comptime {
    //   val x: Option[Int] = Some(1)
    //   x match
    //     case Some(TestExtractors.Triple(a, b, c)) => a + b + c
    //     case _                                    => 0
    // } -> 6,
    // comptime {
    //   val x: Any = -1
    //   x match
    //     case TestExtractors.Triple(a, b, c) => a + b + c
    //     case _                              => 0
    // } -> 0
  )
