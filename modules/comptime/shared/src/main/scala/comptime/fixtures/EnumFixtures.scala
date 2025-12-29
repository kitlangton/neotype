package comptime.fixtures

import _root_.comptime.comptime
import _root_.comptime.comptimeError

// ══════════════════════════════════════════════════════════════════════════════
// Enum fixtures for testing comptime enum support
// These must be in main/ (not test/) so they're compiled before macro expansion
// ══════════════════════════════════════════════════════════════════════════════

enum Row:
  case Top, Middle, Bottom

enum Col:
  case Left, Center, Right

final case class GridPos(row: Row, col: Col)
object GridPos:
  inline def parse(inline s: String): GridPos = comptime {
    val parts = s.split("-")
    val row = parts(0).toLowerCase match
      case "top"    => Row.Top
      case "middle" => Row.Middle
      case "bottom" => Row.Bottom
      case other    => comptimeError(s"Invalid row: $other. Expected top/middle/bottom")
    val col = parts(1).toLowerCase match
      case "left"   => Col.Left
      case "center" => Col.Center
      case "right"  => Col.Right
      case other    => comptimeError(s"Invalid col: $other. Expected left/center/right")
    GridPos(row, col)
  }

// Enum with overridden toString
enum ColorWithToString:
  case Red, Green, Blue

  override def toString: String = this match
    case Red   => "COLOR_RED"
    case Green => "COLOR_GREEN"
    case Blue  => "COLOR_BLUE"

final case class ColorBox(color: ColorWithToString)
object ColorBox:
  inline def make(inline c: String): ColorBox = comptime {
    val color = c.toLowerCase match
      case "red"   => ColorWithToString.Red
      case "green" => ColorWithToString.Green
      case "blue"  => ColorWithToString.Blue
      case other   => comptimeError(s"Unknown color: $other")
    ColorBox(color)
  }

// Parameterized enum
enum HttpStatus(val code: Int):
  case Ok          extends HttpStatus(200)
  case NotFound    extends HttpStatus(404)
  case ServerError extends HttpStatus(500)

final case class StatusBox(status: HttpStatus)
object StatusBox:
  inline def make(inline c: String): StatusBox = comptime {
    val status = c.toLowerCase match
      case "ok"       => HttpStatus.Ok
      case "notfound" => HttpStatus.NotFound
      case "error"    => HttpStatus.ServerError
      case other      => comptimeError(s"Unknown status: $other")
    StatusBox(status)
  }
