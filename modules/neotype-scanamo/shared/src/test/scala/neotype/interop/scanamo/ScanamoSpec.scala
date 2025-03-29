package neotype.interop.scanamo

import neotype.Newtype
import neotype.test.definitions.*
import neotype.test.definitions.SimpleNewtype
import org.scanamo.*
import org.scanamo.DynamoReadError.describe
import zio.test.*

object ScanamoSpec extends ZIOSpecDefault:
  case class Config[A](value: A)

  def spec = suite("ScanamoSpec")(
    suite("NonEmptyString")(
      test("read success") {
        val value  = DynamoValue.fromString("hello")
        val result = DynamoFormat[ValidatedNewtype].read(value)
        assertTrue(result == Right(ValidatedNewtype("hello")))
      },
      test("read failure") {
        val value  = DynamoValue.fromNumber(123)
        val result = DynamoFormat[ValidatedNewtype].read(value)
        assertTrue(result.left.map(describe) == Left("not of type: 'S' was 'DynNum(123)'"))
      },
      test("read validation failure") {
        val value  = DynamoValue.fromString("")
        val result = DynamoFormat[ValidatedNewtype].read(value)
        assertTrue(
          result.left.map(describe) == Left("could not be converted to desired type: String must not be empty")
        )
      },
      test("write success") {
        val value  = ValidatedNewtype("hello")
        val result = DynamoFormat[ValidatedNewtype].write(value)
        assertTrue(result == DynamoValue.fromString("hello"))
      }
    ),
    suite("SimpleNewtype")(
      test("read success") {
        val value  = DynamoValue.fromNumber(123)
        val result = DynamoFormat[SimpleNewtype].read(value)
        assertTrue(result == Right(SimpleNewtype(123)))
      }
    ),
    suite("ValidatedSubtype")(
      test("read success") {
        val value  = DynamoValue.fromString("Hello World!")
        val result = DynamoFormat[ValidatedSubtype].read(value)
        assertTrue(result == Right(ValidatedSubtype("Hello World!")))
      },
      test("read failure") {
        val value  = DynamoValue.fromNumber(123)
        val result = DynamoFormat[ValidatedSubtype].read(value)
        assertTrue(result.left.map(describe) == Left("not of type: 'S' was 'DynNum(123)'"))
      },
      test("read validation failure") {
        val value  = DynamoValue.fromString("too short")
        val result = DynamoFormat[ValidatedSubtype].read(value)
        assertTrue(
          result.left.map(describe) == Left(
            "could not be converted to desired type: String must be longer than 10 characters"
          )
        )
      },
      test("write success") {
        val value  = ValidatedSubtype("Hello World!")
        val result = DynamoFormat[ValidatedSubtype].write(value)
        assertTrue(result == DynamoValue.fromString("Hello World!"))
      }
    ),
    suite("SimpleSubtype")(
      test("read success") {
        val value  = DynamoValue.fromNumber(123)
        val result = DynamoFormat[SimpleSubtype].read(value)
        assertTrue(result == Right(SimpleSubtype(123)))
      },
      test("read failure") {
        val value  = DynamoValue.fromString("not a number")
        val result = DynamoFormat[SimpleSubtype].read(value)
        assertTrue(result.left.map(describe) == Left("not of type: 'N' was 'DynString(not a number)'"))
      }
    )
  )
