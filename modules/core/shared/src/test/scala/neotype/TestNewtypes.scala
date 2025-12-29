package neotype

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

type PositiveIntSubtype = PositiveIntSubtype.Type
object PositiveIntSubtype extends Subtype[Int]:
  override inline def validate(int: Int): Boolean =
    int > 0

type PositiveLongNewtype = PositiveLongNewtype.Type
object PositiveLongNewtype extends Newtype[Long]:
  override inline def validate(long: Long): Boolean = long > 0

type NonEmptyStringNewtype = NonEmptyStringNewtype.Type
object NonEmptyStringNewtype extends Newtype[String]:
  override inline def validate(string: String): Boolean =
    string.length > 0

type NonBlankStringNewtype = NonBlankStringNewtype.Type
object NonBlankStringNewtype extends Newtype[String]:
  override inline def validate(input: String) =
    if input.isBlank then "Must not be empty" else true

type UriStringNewtype = UriStringNewtype.Type
object UriStringNewtype extends Newtype[String]:
  override inline def validate(input: String) =
    if scala.util.Try(new java.net.URI(input)).isFailure then "Must be a valid URI" else true

type UuidStringNewtype = UuidStringNewtype.Type
object UuidStringNewtype extends Newtype[String]:
  override inline def validate(input: String) =
    if !scala.util.Try(java.util.UUID.fromString(input).toString).toOption.contains(input.toLowerCase) then
      "Must be a valid UUID"
    else true

type ShortCircuitOrNewtype = ShortCircuitOrNewtype.Type
object ShortCircuitOrNewtype extends Newtype[Int]:
  override inline def validate(input: Int): Boolean =
    input == 0 || (10 / input) == 5

type ShortCircuitAndNewtype = ShortCircuitAndNewtype.Type
object ShortCircuitAndNewtype extends Newtype[Int]:
  override inline def validate(input: Int): Boolean =
    input != 0 && (10 / input) == 5

type MatchListNewtype = MatchListNewtype.Type
object MatchListNewtype extends Newtype[List[Int]]:
  override inline def validate(input: List[Int]) =
    input match
      case List(a, b, _*) if a < b => true
      case _                       => "Must start with an increasing pair"

type TypedMatchAnyNewtype = TypedMatchAnyNewtype.Type
object TypedMatchAnyNewtype extends Newtype[Any]:
  override inline def validate(input: Any) =
    input match
      case _: String => true
      case _         => "Must be a String"

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

type SpongebobString = SpongebobString.Type
object SpongebobString extends Newtype[String]:
  override inline def validate(value: String): Boolean =
    value.toList
      .foldLeft((true, 0)) { case ((ok, index), char) =>
        if !ok then (false, index)
        else if char.isLetter then
          val isValid = if index % 2 == 0 then char.isUpper else char.isLower
          (isValid, index + 1)
        else (true, 0)
      } match
      case (ok, _) => ok

// Stacked newtypes test fixtures
type Cents = Cents.Type
object Cents extends Newtype[Long]

type NonNegativeCents = NonNegativeCents.Type
object NonNegativeCents extends Subtype[Cents]:
  override inline def validate(input: Cents): Boolean =
    input.unwrap >= 0
