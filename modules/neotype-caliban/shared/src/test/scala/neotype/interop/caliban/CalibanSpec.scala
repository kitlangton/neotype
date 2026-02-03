package neotype.interop.caliban

import caliban.Value.IntValue
import caliban.Value.StringValue
import caliban.schema.*
import neotype.test.definitions.*
import zio.test.*

import scala.compiletime.summonInline

object CalibanSpec extends ZIOSpecDefault:
  def spec = suite("CirceJsonSpec")(
    suite("NonEmptyString")(
      test("Schema") {
        val schema = summonInline[Schema[Any, ValidatedNewtype]]
        assertTrue(schema.toType_().name.contains("String"))
      },
      test("ArgBuilder success") {
        val argBuilder = summonInline[ArgBuilder[ValidatedNewtype]]
        assertTrue(argBuilder.build(StringValue("hello")) == Right(ValidatedNewtype("hello")))
      },
      test("ArgBuilder failure") {
        val argBuilder = summonInline[ArgBuilder[ValidatedNewtype]]
        assertTrue(argBuilder.build(StringValue("")).left.map(_.msg) == Left("String must not be empty"))
      }
    ),
    suite("SubtypeLongString")(
      test("Schema") {
        val schema = summonInline[Schema[Any, ValidatedSubtype]]
        assertTrue(schema.toType_().name.contains("String"))
      },
      test("ArgBuilder success") {
        val argBuilder = summonInline[ArgBuilder[ValidatedSubtype]]
        assertTrue(argBuilder.build(StringValue("hello world")) == Right(ValidatedSubtype("hello world")))
      },
      test("ArgBuilder failure") {
        val argBuilder = summonInline[ArgBuilder[ValidatedSubtype]]
        assertTrue(
          argBuilder.build(StringValue("hello")).left.map(_.msg) == Left("String must be longer than 10 characters")
        )
      }
    ),
    suite("SimpleNewtype")(
      test("Schema") {
        val schema = summonInline[Schema[Any, SimpleNewtype]]
        assertTrue(schema.toType_().name.contains("Int"))
      },
      test("ArgBuilder success") {
        val argBuilder = summonInline[ArgBuilder[SimpleNewtype]]
        assertTrue(argBuilder.build(IntValue(123)) == Right(SimpleNewtype(123)))
      }
    ),
    suite("SimpleSubtype")(
      test("Schema") {
        val schema = summonInline[Schema[Any, SimpleSubtype]]
        assertTrue(schema.toType_().name.contains("Int"))
      },
      test("ArgBuilder success") {
        val argBuilder = summonInline[ArgBuilder[SimpleSubtype]]
        assertTrue(argBuilder.build(IntValue(123)) == Right(SimpleNewtype(123)))
      }
    )
  )
