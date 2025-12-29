package comptime

import zio.test.*

object OptionSpec extends ZIOSpecDefault:
  val spec =
    suite("OptionSpec (comptime)")(
      evalTests.map { case (actual, expected) =>
        test(s"comptime($actual) == $expected") {
          assertTrue(actual == expected)
        }
      }
    )

  lazy val evalTests = List(
    comptime(Option(1).collect { case x if x > 0 => x + 1 }) -> Some(2),
    comptime(Option(1).collect { case x if x > 10 => x })    -> None,
    comptime(Option.empty[Int].collect { case 1 => 2 })      -> None,

    // Nullary / predicates
    comptime(Option(1).isEmpty)             -> false,
    comptime(Option.empty[Int].isEmpty)     -> true,
    comptime(Option(1).nonEmpty)            -> true,
    comptime(Option.empty[Int].nonEmpty)    -> false,
    comptime(Option(1).isDefined)           -> true,
    comptime(Option.empty[Int].isDefined)   -> false,
    comptime(Option(1).contains(1))         -> true,
    comptime(Option(1).contains(2))         -> false,
    comptime(Option.empty[Int].contains(1)) -> false,

    // Option.when/unless with by-name arg
    comptime(Option.when(true)(1))       -> Some(1),
    comptime(Option.when(false)(1 / 0))  -> None,
    comptime(Option.unless(false)(1))    -> Some(1),
    comptime(Option.unless(true)(1 / 0)) -> None,

    // getOrElse (by-name default)
    comptime(Option(1).getOrElse(0))         -> 1,
    comptime(Option.empty[Int].getOrElse(0)) -> 0,
    comptime(Option(1).getOrElse(1 / 0))     -> 1,

    // orElse (by-name alternative)
    comptime(Option(1).orElse(Option(2)))         -> Some(1),
    comptime(Option.empty[Int].orElse(Option(2))) -> Some(2),
    comptime(Option(1).orElse(Option(1 / 0)))     -> Some(1),

    // toRight / toLeft (by-name side)
    comptime(Option(1).toRight("err"))         -> Right(1),
    comptime(Option.empty[Int].toRight("err")) -> Left("err"),
    comptime(Option(1).toRight(1 / 0))         -> Right(1),
    comptime(Option(1).toLeft("err"))          -> Left(1),
    comptime(Option.empty[Int].toLeft("err"))  -> Right("err"),
    comptime(Option(1).toLeft(1 / 0))          -> Left(1),

    // fold (by-name ifEmpty)
    comptime(Option(1).fold(0)(_ + 1))         -> 2,
    comptime(Option.empty[Int].fold(0)(_ + 1)) -> 0,
    comptime(Option(1).fold(1 / 0)(_ + 1))     -> 2,

    // filterNot
    comptime(Option(1).filterNot(_ > 0))         -> None,
    comptime(Option(1).filterNot(_ < 0))         -> Some(1),
    comptime(Option.empty[Int].filterNot(_ > 0)) -> None,

    // flatten
    comptime(Option(Some(1)).flatten)           -> Some(1),
    comptime(Option(None).flatten)              -> None,
    comptime(Option.empty[Option[Int]].flatten) -> None,

    // get
    // TODO: Option.get is not yet supported in comptime
    // comptime(Option(1).get)                        -> 1,
    // comptime(Try(Option.empty[Int].get).isFailure) -> true,

    // iterator / toList / toSeq
    comptime(Option(1).iterator.toList)         -> List(1),
    comptime(Option.empty[Int].iterator.toList) -> List(),
    comptime(Option(1).toList)                  -> List(1),
    comptime(Option.empty[Int].toList)          -> List(),
    comptime(Option(1).toSeq.toList)            -> List(1),
    comptime(Option.empty[Int].toSeq.toList)    -> List(),

    // orNull (requires implicit evidence)
    comptime(Option("a").orNull == "a")           -> true,
    comptime(Option.empty[String].orNull == null) -> true,

    // zip
    comptime(Some(1).zip(Some("a")))           -> Some((1, "a")),
    comptime(Some(1).zip(None))                -> None,
    comptime(Option.empty[Int].zip(Some("a"))) -> None,

    // sum / product for Option
    comptime(Option(2).sum)                     -> 2,
    comptime(Some(3).sum)                       -> 3,
    comptime(Option(2L).sum)                    -> 2L,
    comptime(Option(2.5).sum)                   -> 2.5,
    comptime(Option(2.5f).sum)                  -> 2.5f,
    comptime(Option(BigInt(5)).sum)             -> BigInt(5),
    comptime(Option(BigDecimal("1.25")).sum)    -> BigDecimal("1.25"),
    comptime(Option(3).product)                 -> 3,
    comptime(Some(4).product)                   -> 4,
    comptime(Option(BigInt(5)).product)         -> BigInt(5),
    comptime(Option(BigDecimal("2.5")).product) -> BigDecimal("2.5"),
    // Empty Option sum/product (returns type-appropriate zero/one)
    comptime(Option.empty[Int].sum)     -> 0,
    comptime(Option.empty[Int].product) -> 1
  )
