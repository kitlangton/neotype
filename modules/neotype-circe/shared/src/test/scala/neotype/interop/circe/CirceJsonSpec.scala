package neotype.interop.circe

import io.circe.*
import io.circe.syntax.*
import neotype.*
import neotype.interop.circe.given
import neotype.test.*
import neotype.test.definitions.*
import zio.test.*

// Circe doesn't have a unified Codec type that's commonly used,
// so we create a stub combining Decoder and Encoder
final case class CirceCodec[A](decoder: Decoder[A], encoder: Encoder[A])
object CirceCodec:
  given [A](using d: Decoder[A], e: Encoder[A]): CirceCodec[A] = CirceCodec(d, e)

object CirceLibrary extends JsonLibrary[CirceCodec]:
  def decode[A](json: String)(using codec: CirceCodec[A]): Either[String, A] =
    given Decoder[A] = codec.decoder
    io.circe.parser.decode[A](json).left.map(_.getMessage)

  def encode[A](value: A)(using codec: CirceCodec[A]): String =
    given Encoder[A] = codec.encoder
    value.asJson.noSpaces

// Manual codec definitions (circe-generic not available)
given Decoder[Composite] = Decoder.instance { c =>
  for
    newtype       <- c.get[ValidatedNewtype]("newtype")
    simpleNewtype <- c.get[SimpleNewtype]("simpleNewtype")
    subtype       <- c.get[ValidatedSubtype]("subtype")
    simpleSubtype <- c.get[SimpleSubtype]("simpleSubtype")
  yield Composite(newtype, simpleNewtype, subtype, simpleSubtype)
}

given Encoder[Composite] = Encoder.instance { c =>
  Json.obj(
    "newtype"       -> c.newtype.asJson,
    "simpleNewtype" -> c.simpleNewtype.asJson,
    "subtype"       -> c.subtype.asJson,
    "simpleSubtype" -> c.simpleSubtype.asJson
  )
}

given Decoder[OptionalHolder] = Decoder.instance { c =>
  c.get[OptionalString]("value").map(OptionalHolder.apply)
}

given Encoder[OptionalHolder] = Encoder.instance { h =>
  Json.obj("value" -> h.value.asJson)
}

given Decoder[ListHolder] = Decoder.instance { c =>
  c.get[List[ValidatedNewtype]]("items").map(ListHolder.apply)
}

given Encoder[ListHolder] = Encoder.instance { h =>
  Json.obj("items" -> h.items.asJson)
}

object CirceJsonSpec extends JsonLibrarySpec[CirceCodec]("Circe", CirceLibrary):
  type SimpleStringNewtype = SimpleStringNewtype.Type
  object SimpleStringNewtype extends Newtype[String]

  type PositiveIntKey = PositiveIntKey.Type
  object PositiveIntKey extends Newtype[Int]:
    override inline def validate(value: Int): Boolean = value > 0

  type NewtypeValidatedSubtype = NewtypeValidatedSubtype.Type
  object NewtypeValidatedSubtype extends Newtype[ValidatedSubtype]

  type TwiceValidatedSubtype = TwiceValidatedSubtype.Type
  object TwiceValidatedSubtype extends Subtype[ValidatedSubtype]:
    override inline def validate(input: ValidatedSubtype): Boolean | String =
      if input.length > 15 then true else "String must be longer than 15 characters"

  override protected def optionalHolderCodec: Option[CirceCodec[OptionalHolder]] =
    Some(summon[CirceCodec[OptionalHolder]])

  override protected def listHolderCodec: Option[CirceCodec[ListHolder]] =
    Some(summon[CirceCodec[ListHolder]])

  override protected def additionalSuites: List[Spec[Any, Nothing]] = List(
    suite("Map with wrapped key")(
      test("decode success") {
        val json   = """{"hello":1,"world":2}"""
        val parsed = parser.decode[Map[ValidatedNewtype, Int]](json)
        assertTrue(
          parsed == Right(Map(ValidatedNewtype("hello") -> 1, ValidatedNewtype("world") -> 2))
        )
      },
      test("decode failure - empty key fails validation") {
        val parsed = parser.decode[Map[ValidatedNewtype, Int]]("""{"":1}""")
        assertTrue(parsed.isLeft)
      },
      test("encode") {
        val json = Map(ValidatedNewtype("hello") -> 1, ValidatedNewtype("meaning") -> 42).asJson
        assertTrue(json == Json.obj("hello" -> 1.asJson, "meaning" -> 42.asJson))
      },
      test("simple String keys roundtrip with wrapped values") {
        val expected = Map(SimpleStringNewtype("hello") -> ValidatedNewtype("world"))
        val json     = expected.asJson
        assertTrue(
          json == Json.obj("hello" -> "world".asJson),
          parser.decode[Map[SimpleStringNewtype, ValidatedNewtype]](json.noSpaces) == Right(expected)
        )
      },
      test("validated subtype keys roundtrip") {
        val expected = Map(ValidatedSubtype("hello world") -> 1)
        val json     = expected.asJson
        assertTrue(
          json == Json.obj("hello world" -> 1.asJson),
          parser.decode[Map[ValidatedSubtype, Int]](json.noSpaces) == Right(expected)
        )
      },
      test("invalid validated subtype key") {
        assertTrue(parser.decode[Map[ValidatedSubtype, Int]]("""{"short":1}""").isLeft)
      },
      test("validated integer keys roundtrip") {
        val expected = Map(PositiveIntKey(42) -> "answer")
        val json     = expected.asJson
        assertTrue(
          json == Json.obj("42" -> "answer".asJson),
          parser.decode[Map[PositiveIntKey, String]](json.noSpaces) == Right(expected)
        )
      },
      test("integer keys reject underlying parse failure and wrapped validation failure") {
        assertTrue(
          parser.decode[Map[PositiveIntKey, String]]("""{"not-an-int":"value"}""").isLeft,
          parser.decode[Map[PositiveIntKey, String]]("""{"0":"value"}""").isLeft
        )
      },
      test("simple newtype over validated subtype keys roundtrip") {
        val expected = Map(NewtypeValidatedSubtype(ValidatedSubtype("long enough key")) -> 1)
        val json     = expected.asJson
        assertTrue(
          json == Json.obj("long enough key" -> 1.asJson),
          parser.decode[Map[NewtypeValidatedSubtype, Int]](json.noSpaces) == Right(expected)
        )
      },
      test("simple outer wrapper preserves inner key validation") {
        assertTrue(parser.decode[Map[NewtypeValidatedSubtype, Int]]("""{"short":1}""").isLeft)
      },
      test("twice validated subtype keys roundtrip") {
        val key      = TwiceValidatedSubtype.makeOrThrow(ValidatedSubtype("this key is long enough"))
        val expected = Map(key -> 1)
        val json     = expected.asJson
        assertTrue(
          json == Json.obj("this key is long enough" -> 1.asJson),
          parser.decode[Map[TwiceValidatedSubtype, Int]](json.noSpaces) == Right(expected)
        )
      },
      test("twice validated subtype rejects inner and outer validation failures") {
        assertTrue(
          parser.decode[Map[TwiceValidatedSubtype, Int]]("""{"short":1}""").isLeft,
          parser.decode[Map[TwiceValidatedSubtype, Int]]("""{"twelve chars":1}""").isLeft
        )
      },
      test("ordinary String and integer maps remain unchanged") {
        val strings  = Map("hello" -> 1)
        val integers = Map(42 -> "answer")
        assertTrue(
          strings.asJson == Json.obj("hello" -> 1.asJson),
          integers.asJson == Json.obj("42" -> "answer".asJson),
          parser.decode[Map[String, Int]](strings.asJson.noSpaces) == Right(strings),
          parser.decode[Map[Int, String]](integers.asJson.noSpaces) == Right(integers)
        )
      }
    )
  )
