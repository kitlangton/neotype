package neotype.test

import neotype.test.definitions.*
import zio.test.*

/** How a JSON library encodes None/null Option values. */
enum NoneEncoding:
  /** Encode as explicit null: {"value":null} */
  case AsNull

  /** Omit the field entirely: {} */
  case OmitField

/** How a JSON library encodes empty lists. */
enum EmptyListEncoding:
  /** Encode as empty array: {"items":[]} */
  case AsEmptyArray

  /** Omit the field entirely: {} */
  case OmitField

trait JsonLibrary[Codec[_]]:
  def decode[A](json: String)(using codec: Codec[A]): Either[String, A]
  def encode[A](value: A)(using codec: Codec[A]): String

/** Base spec for JSON library integrations.
  *
  * Provides exhaustive tests for:
  *   - ValidatedNewtype (String with non-empty validation)
  *   - ValidatedSubtype (String with length > 10 validation)
  *   - SimpleNewtype (Int, no validation)
  *   - SimpleSubtype (Int, no validation)
  *   - Composite (case class with all above)
  *   - OptionalHolder (Option[String] newtype for null handling)
  *
  * To use, extend this trait and provide:
  *   - A JsonLibrary implementation
  *   - Codec instances for all test types
  */
trait JsonLibrarySpec[Codec[_]](
    name: String,
    val library: JsonLibrary[Codec]
)(using
    compositeCodec: Codec[Composite],
    validatedNewtypeCodec: Codec[ValidatedNewtype],
    validatedSubtypeCodec: Codec[ValidatedSubtype],
    simpleNewtypeCodec: Codec[SimpleNewtype],
    simpleSubtypeCodec: Codec[SimpleSubtype]
) extends ZIOSpecDefault:

  /** Override to provide OptionalHolder codec for Option handling tests. If not
    * provided, Option tests will be skipped.
    */
  protected def optionalHolderCodec: Option[Codec[OptionalHolder]] = None

  /** Override to provide ListHolder codec for collection tests. If not
    * provided, collection tests will be skipped.
    */
  protected def listHolderCodec: Option[Codec[ListHolder]] = None

  /** How this library encodes None values in Option fields. */
  protected def noneEncoding: NoneEncoding = NoneEncoding.AsNull

  /** How this library encodes empty lists. */
  protected def emptyListEncoding: EmptyListEncoding = EmptyListEncoding.AsEmptyArray

  /** Additional library-specific test suites to include. */
  protected def additionalSuites: List[Spec[Any, Nothing]] = Nil

  final def spec: Spec[Any, Nothing] =
    suite(s"${name}JsonSpec")(
      List(
        validatedNewtypeSuite,
        validatedSubtypeSuite,
        simpleNewtypeSuite,
        simpleSubtypeSuite,
        compositeSuite
      ) ++ optionalSuite.toList ++ collectionSuite.toList ++ additionalSuites: _*
    )

  private def validatedNewtypeSuite = suite("ValidatedNewtype")(
    test("decode success") {
      val json   = """ "hello" """
      val parsed = library.decode[ValidatedNewtype](json)
      assertTrue(parsed == Right(ValidatedNewtype("hello")))
    },
    test("decode failure - empty string") {
      val json   = """ "" """
      val parsed = library.decode[ValidatedNewtype](json)
      assertTrue(
        parsed.isLeft,
        parsed.left.exists(_.contains("String must not be empty"))
      )
    },
    test("encode") {
      val json = library.encode(ValidatedNewtype("hello"))
      assertTrue(json == """"hello"""")
    }
  )

  private def validatedSubtypeSuite = suite("ValidatedSubtype")(
    test("decode success") {
      val json   = """ "hello world" """
      val parsed = library.decode[ValidatedSubtype](json)
      assertTrue(parsed == Right(ValidatedSubtype("hello world")))
    },
    test("decode failure - too short") {
      val json   = """ "hello" """
      val parsed = library.decode[ValidatedSubtype](json)
      assertTrue(
        parsed.isLeft,
        parsed.left.exists(_.contains("String must be longer than 10 characters"))
      )
    },
    test("encode") {
      val json = library.encode(ValidatedSubtype("hello world"))
      assertTrue(json == """"hello world"""")
    }
  )

  private def simpleNewtypeSuite = suite("SimpleNewtype")(
    test("decode success") {
      val json   = """ 123 """
      val parsed = library.decode[SimpleNewtype](json)
      assertTrue(parsed == Right(SimpleNewtype(123)))
    },
    test("decode failure - wrong type") {
      val json   = """ "hello" """
      val parsed = library.decode[SimpleNewtype](json)
      assertTrue(parsed.isLeft)
    },
    test("encode") {
      val json = library.encode(SimpleNewtype(123))
      assertTrue(json == "123")
    }
  )

  private def simpleSubtypeSuite = suite("SimpleSubtype")(
    test("decode success") {
      val json   = """ 123 """
      val parsed = library.decode[SimpleSubtype](json)
      assertTrue(parsed == Right(SimpleSubtype(123)))
    },
    test("decode failure - wrong type") {
      val json   = """ "WHOOPS" """
      val parsed = library.decode[SimpleSubtype](json)
      assertTrue(parsed.isLeft)
    },
    test("encode") {
      val json = library.encode(SimpleSubtype(123))
      assertTrue(json == "123")
    }
  )

  private def compositeSuite = suite("Composite")(
    test("decode success") {
      val json =
        """ { "newtype": "hello", "simpleNewtype": 123, "subtype": "hello world", "simpleSubtype": 123 } """
      val parsed = library.decode[Composite](json)
      assertTrue(
        parsed == Right(
          Composite(
            ValidatedNewtype("hello"),
            SimpleNewtype(123),
            ValidatedSubtype("hello world"),
            SimpleSubtype(123)
          )
        )
      )
    },
    test("decode failure - validation error in nested field") {
      val json   = """ { "newtype": "", "simpleNewtype": 123, "subtype": "hello", "simpleSubtype": 123 } """
      val parsed = library.decode[Composite](json)
      assertTrue(
        parsed.isLeft,
        parsed.left.exists(_.contains("String must not be empty"))
      )
    },
    test("encode") {
      val json = library.encode(
        Composite(
          ValidatedNewtype("hello"),
          SimpleNewtype(123),
          ValidatedSubtype("hello world"),
          SimpleSubtype(123)
        )
      )
      assertTrue(
        json == """{"newtype":"hello","simpleNewtype":123,"subtype":"hello world","simpleSubtype":123}"""
      )
    }
  )

  private def optionalSuite: Option[Spec[Any, Nothing]] =
    optionalHolderCodec.map { implicit codec =>
      suite("OptionalHolder (Option newtype)")(
        test("decode Some") {
          val json   = """{"value":"hello"}"""
          val parsed = library.decode[OptionalHolder](json)
          assertTrue(parsed == Right(OptionalHolder(OptionalString(Some("hello")))))
        },
        test("decode None (null)") {
          val json   = """{"value":null}"""
          val parsed = library.decode[OptionalHolder](json)
          assertTrue(parsed == Right(OptionalHolder(OptionalString(None))))
        },
        test("encode Some") {
          val json = library.encode(OptionalHolder(OptionalString(Some("hello"))))
          assertTrue(json == """{"value":"hello"}""")
        },
        test("encode None") {
          val json = library.encode(OptionalHolder(OptionalString(None)))
          val expected = noneEncoding match
            case NoneEncoding.AsNull    => """{"value":null}"""
            case NoneEncoding.OmitField => "{}"
          assertTrue(json == expected)
        }
      )
    }

  private def collectionSuite: Option[Spec[Any, Nothing]] =
    listHolderCodec.map { implicit codec =>
      suite("ListHolder (List[ValidatedNewtype])")(
        test("decode list with valid items") {
          val json   = """{"items":["hello","world"]}"""
          val parsed = library.decode[ListHolder](json)
          assertTrue(parsed == Right(ListHolder(List(ValidatedNewtype("hello"), ValidatedNewtype("world")))))
        },
        test("decode empty list") {
          val json   = """{"items":[]}"""
          val parsed = library.decode[ListHolder](json)
          assertTrue(parsed == Right(ListHolder(List())))
        },
        test("decode list with invalid item fails") {
          val json   = """{"items":["hello",""]}"""
          val parsed = library.decode[ListHolder](json)
          assertTrue(
            parsed.isLeft,
            parsed.left.exists(_.contains("String must not be empty"))
          )
        },
        test("encode list") {
          val json = library.encode(ListHolder(List(ValidatedNewtype("hello"), ValidatedNewtype("world"))))
          assertTrue(json == """{"items":["hello","world"]}""")
        },
        test("encode empty list") {
          val json = library.encode(ListHolder(List()))
          val expected = emptyListEncoding match
            case EmptyListEncoding.AsEmptyArray => """{"items":[]}"""
            case EmptyListEncoding.OmitField    => "{}"
          assertTrue(json == expected)
        }
      )
    }
