package neotype

import scala.quoted.FromExpr
import scala.util.matching.Regex

object NewtypeWithoutValidation extends Newtype[String]:
  override inline def validate(string: String): Boolean = true

type RegexNewtype = RegexNewtype.Type
object RegexNewtype extends Newtype[String]:
  override inline def validate(string: String): Boolean =
    string.matches("^[a-zA-Z0-9]*$")

  given Show[RegexNewtype] with
    def show(value: RegexNewtype): String = unwrap(value)

  extension (value: RegexNewtype) //
    def length: Int = unwrap(value).length

type PositiveIntNewtype = PositiveIntNewtype.Type
object PositiveIntNewtype extends Newtype[Int]:
  override inline def validate(int: Int): Boolean =
    int > 0

type PositiveLongNewtype = PositiveLongNewtype.Type
object PositiveLongNewtype extends Newtype[Long]:
  override inline def validate(long: Long): Boolean = long > 0

type NonEmptyStringNewtype = NonEmptyStringNewtype.Type
object NonEmptyStringNewtype extends Newtype[String]:
  override inline def validate(string: String): Boolean =
    string.length > 0

// Newtype for a custom case class
final case class Person(name: String, age: Int)

object Person:
  //   def unapply(x: Expr[T])(using Quotes): Option[T]
  import scala.quoted.*

  given FromExpr[Person] with
    def unapply(x: Expr[Person])(using Quotes): Option[Person] =
      x match
        case '{ Person(${ Expr(name) }, ${ Expr(age) }) } =>
          Some(Person(name, age))
        case _ => None

object PersonNewtype extends Newtype[Person]:
  override inline def validate(person: Person): Boolean =
    person.name.length > 0 && person.age > 0

object VariousStringNewtype extends Newtype[String]:
  override inline def validate(string: String): Boolean =
    string.startsWith("a") && string.endsWith("z") &&
      string.length > 0 && string.contains("b")
