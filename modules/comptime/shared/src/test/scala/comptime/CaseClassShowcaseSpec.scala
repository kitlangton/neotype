package comptime

import zio.test.*

// ══════════════════════════════════════════════════════════════════════════════
// SHOWCASE: Compile-time case class construction from string parsing
// ══════════════════════════════════════════════════════════════════════════════

// Semantic versioning: "1.2.3" → SemVer(1, 2, 3)
final case class SemVer(major: Int, minor: Int, patch: Int)
object SemVer:
  inline def parse(inline s: String): SemVer = comptime {
    val parts = s.split("\\.")
    SemVer(parts(0).toInt, parts(1).toInt, parts(2).toInt)
  }

// ══════════════════════════════════════════════════════════════════════════════
// SHOWCASE: Custom error messages with comptimeError
// ══════════════════════════════════════════════════════════════════════════════

// Time: "14:30" → Time(14, 30) with validation
final case class Time(hour: Int, minute: Int):
  override def toString: String = f"$hour%02d:$minute%02d"

object Time:
  inline def parse(inline s: String): Time = comptime {
    val parts = s.split(":")
    if parts.length != 2 then comptimeError(s"Invalid time format: '$s'. Expected HH:MM")
    val hour   = parts(0).toInt
    val minute = parts(1).toInt
    if hour < 0 || hour > 23 then comptimeError(s"Invalid hour: $hour. Must be 0-23")
    if minute < 0 || minute > 59 then comptimeError(s"Invalid minute: $minute. Must be 0-59")
    Time(hour, minute)
  }

// Port: validated port number
final case class Port(value: Int)
object Port:
  inline def apply(inline s: String): Port = comptime {
    val n = s.toInt
    if n < 1 || n > 65535 then comptimeError(s"Invalid port: $n. Must be 1-65535")
    Port(n)
  }

// ══════════════════════════════════════════════════════════════════════════════
// SHOWCASE: Enums in case classes
// ══════════════════════════════════════════════════════════════════════════════

// Enums defined in fixtures (must be in main/ for comptime)
import _root_.comptime.fixtures.{GridPos, Row, Col}

// Chess square: "e4" → ChessSquare('e', 4)
final case class ChessSquare(file: Char, rank: Int)
object ChessSquare:
  inline def apply(inline notation: String): ChessSquare = comptime {
    val file = notation.charAt(0)
    val rank = notation.charAt(1).toString.toInt
    ChessSquare(file, rank)
  }

// RGB Color from simple format: "255,87,51" → RgbColor(255, 87, 51)
final case class RgbColor(r: Int, g: Int, b: Int)
object RgbColor:
  inline def parse(inline rgb: String): RgbColor = comptime {
    val parts = rgb.split(",")
    RgbColor(parts(0).trim.toInt, parts(1).trim.toInt, parts(2).trim.toInt)
  }

// IP Address: "192.168.1.1" → IPv4(192, 168, 1, 1)
final case class IPv4(a: Int, b: Int, c: Int, d: Int):
  override def toString: String = s"$a.$b.$c.$d"
object IPv4:
  inline def parse(inline s: String): IPv4 = comptime {
    val parts = s.split("\\.")
    IPv4(parts(0).toInt, parts(1).toInt, parts(2).toInt, parts(3).toInt)
  }

// Point: "3,4" → Point(3, 4)
final case class Point(x: Int, y: Int)
object Point:
  inline def parse(inline s: String): Point = comptime {
    val parts = s.split(",")
    Point(parts(0).trim.toInt, parts(1).trim.toInt)
  }

// Email: "user@domain.com" → Email("user", "domain.com")
final case class Email(user: String, domain: String)
object Email:
  inline def parse(inline s: String): Email = comptime {
    val parts = s.split("@")
    Email(parts(0), parts(1))
  }

// Range: "1..10" → IntRange(1, 10)
final case class IntRange(start: Int, end: Int)
object IntRange:
  inline def parse(inline s: String): IntRange = comptime {
    val parts = s.split("\\.\\.")
    IntRange(parts(0).toInt, parts(1).toInt)
  }

object CaseClassShowcaseSpec extends ZIOSpecDefault:
  val spec =
    suite("CaseClassShowcaseSpec")(
      suite("SemVer parsing")(
        test("parse valid semver") {
          val v = SemVer.parse("1.2.3")
          assertTrue(
            v.major == 1,
            v.minor == 2,
            v.patch == 3
          )
        },
        test("parse zero version") {
          val v = SemVer.parse("0.0.1")
          assertTrue(v == SemVer(0, 0, 1))
        },
        test("parse large version") {
          val v = SemVer.parse("10.20.30")
          assertTrue(v == SemVer(10, 20, 30))
        }
      ),
      suite("GridPos parsing (enums)")(
        test("parse top-left") {
          val pos = GridPos.parse("top-left")
          assertTrue(pos == GridPos(Row.Top, Col.Left))
        },
        test("parse middle-center") {
          val pos = GridPos.parse("middle-center")
          assertTrue(pos == GridPos(Row.Middle, Col.Center))
        },
        test("parse bottom-right") {
          val pos = GridPos.parse("bottom-right")
          assertTrue(pos == GridPos(Row.Bottom, Col.Right))
        }
      ),
      suite("ChessSquare parsing")(
        test("parse e4") {
          val sq = ChessSquare("e4")
          assertTrue(sq.file == 'e', sq.rank == 4)
        },
        test("parse corners") {
          assertTrue(
            ChessSquare("a1") == ChessSquare('a', 1),
            ChessSquare("h8") == ChessSquare('h', 8),
            ChessSquare("a8") == ChessSquare('a', 8),
            ChessSquare("h1") == ChessSquare('h', 1)
          )
        }
      ),
      suite("RgbColor parsing")(
        test("parse red") {
          val c = RgbColor.parse("255,0,0")
          assertTrue(c == RgbColor(255, 0, 0))
        },
        test("parse with spaces") {
          val c = RgbColor.parse("255, 87, 51")
          assertTrue(c == RgbColor(255, 87, 51))
        }
      ),
      suite("IPv4 parsing")(
        test("parse localhost") {
          val ip = IPv4.parse("127.0.0.1")
          assertTrue(ip == IPv4(127, 0, 0, 1))
        },
        test("parse private network") {
          val ip = IPv4.parse("192.168.1.1")
          assertTrue(ip == IPv4(192, 168, 1, 1))
        }
      ),
      suite("Point parsing")(
        test("parse origin") {
          val p = Point.parse("0,0")
          assertTrue(p == Point(0, 0))
        },
        test("parse with spaces") {
          val p = Point.parse("3, 4")
          assertTrue(p == Point(3, 4))
        }
      ),
      suite("Email parsing")(
        test("parse simple email") {
          val e = Email.parse("user@example.com")
          assertTrue(e == Email("user", "example.com"))
        },
        test("parse email with dots") {
          val e = Email.parse("first.last@company.co.uk")
          assertTrue(e == Email("first.last", "company.co.uk"))
        }
      ),
      suite("IntRange parsing")(
        test("parse range") {
          val r = IntRange.parse("1..10")
          assertTrue(r == IntRange(1, 10))
        },
        test("parse negative range") {
          val r = IntRange.parse("-5..5")
          assertTrue(r == IntRange(-5, 5))
        }
      ),
      suite("comptimeError compile-time errors")(
        test("GridPos invalid row shows custom error") {
          import scala.compiletime.testing.*
          val errors = typeCheckErrors("""_root_.comptime.fixtures.GridPos.parse("invalid-left")""")
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Invalid row: invalid")
          )
        },
        test("GridPos invalid col shows custom error") {
          import scala.compiletime.testing.*
          val errors = typeCheckErrors("""_root_.comptime.fixtures.GridPos.parse("top-wrong")""")
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Invalid col: wrong")
          )
        },
        test("Port invalid value shows custom error") {
          import scala.compiletime.testing.*
          val errors = typeCheckErrors("""Port("99999")""")
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Invalid port: 99999")
          )
        },
        test("Port negative value shows custom error") {
          import scala.compiletime.testing.*
          val errors = typeCheckErrors("""Port("-1")""")
          assertTrue(
            errors.nonEmpty,
            errors.head.message.contains("Invalid port: -1")
          )
        }
      )
    )
