package neotype.interop.scanamo

import neotype.Newtype
import neotype.test.definitions.*
import neotype.test.definitions.SimpleNewtype
import zio.test.*
import org.scanamo.*
import org.scanamo.DynamoReadError.describe

object ScanamoSpec extends ZIOSpecDefault:
  case class Config[A](value: A)

  def spec = suite("ScanamoSpec")(
    suite("NonEmptyString")(
      test("read success") {
        val value = DynamoValue.fromString("hello")
        val result = DynamoFormat[ValidatedNewtype].read(value)
        assertTrue(result == Right(ValidatedNewtype("hello")))
      },
      test("read failure") {
        val value = DynamoValue.fromNumber(123)
        val result = DynamoFormat[ValidatedNewtype].read(value)
        assertTrue(result.left.map(describe) == Left("not of type: 'S' was 'DynNum(123)'"))
      },
      test("read validation failure") {
        val value = DynamoValue.fromString("")
        val result = DynamoFormat[ValidatedNewtype].read(value)
        assertTrue(result.left.map(describe) == Left("could not be converted to desired type: String must not be empty"))
      },
      test("write success") {
        val value = ValidatedNewtype("hello")
        val result = DynamoFormat[ValidatedNewtype].write(value)
        assertTrue(result == DynamoValue.fromString("hello"))
      }
    ),
    suite("SimpleNewtype")(
      test("read success") {
        val value = DynamoValue.fromNumber(123)
        val result = DynamoFormat[SimpleNewtype].read(value)
        assertTrue(result == Right(SimpleNewtype(123)))
      }
    )
  )
