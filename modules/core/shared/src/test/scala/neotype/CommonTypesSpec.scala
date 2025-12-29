package neotype

import neotype.common.*
import zio.test.*

import scala.compiletime.testing.*

object CommonTypesSpec extends ZIOSpecDefault:
  val spec = suiteAll("CommonTypesSpec") {

    suiteAll("HexString") {
      test("valid hex lowercase") {
        val res = HexString("abc123")
        assertTrue(res == "abc123")
      }

      test("valid hex uppercase") {
        val res = HexString("ABC123")
        assertTrue(res == "ABC123")
      }

      test("valid hex mixed case") {
        val res = HexString("AbC123")
        assertTrue(res == "AbC123")
      }

      test("invalid hex - contains g") {
        val res = typeCheckErrors("""HexString("abcg")""").head
        assertTrue(res.message contains "hexadecimal")
      }

      test("invalid hex - empty") {
        val res = typeCheckErrors("""HexString("")""").head
        assertTrue(res.message contains "cannot be empty")
      }
    }

    suiteAll("EmailString") {
      test("valid email") {
        val res = EmailString("test@example.com")
        assertTrue(res == "test@example.com")
      }

      test("valid email with subdomain") {
        val res = EmailString("user@mail.example.com")
        assertTrue(res == "user@mail.example.com")
      }

      test("invalid email - no @") {
        val res = typeCheckErrors("""EmailString("invalid")""").head
        assertTrue(res.message contains "valid email")
      }

      test("invalid email - no domain dot") {
        val res = typeCheckErrors("""EmailString("test@example")""").head
        assertTrue(res.message contains "valid email")
      }

      test("invalid email - empty local part") {
        val res = typeCheckErrors("""EmailString("@example.com")""").head
        assertTrue(res.message contains "valid email")
      }
    }

    suiteAll("PositiveInt") {
      test("valid positive") {
        val res = PositiveInt(1)
        assertTrue(res == 1)
      }

      test("invalid zero") {
        val res = typeCheckErrors("""PositiveInt(0)""").head
        assertTrue(res.message contains "positive")
      }

      test("invalid negative") {
        val res = typeCheckErrors("""PositiveInt(-1)""").head
        assertTrue(res.message contains "positive")
      }
    }

    suiteAll("NonNegativeInt") {
      test("valid positive") {
        val res = NonNegativeInt(1)
        assertTrue(res == 1)
      }

      test("valid zero") {
        val res = NonNegativeInt(0)
        assertTrue(res == 0)
      }

      test("invalid negative") {
        val res = typeCheckErrors("""NonNegativeInt(-1)""").head
        assertTrue(res.message contains "non-negative")
      }
    }

    suiteAll("NegativeInt") {
      test("valid negative") {
        val res = NegativeInt(-1)
        assertTrue(res == -1)
      }

      test("invalid zero") {
        val res = typeCheckErrors("""NegativeInt(0)""").head
        assertTrue(res.message contains "negative")
      }

      test("invalid positive") {
        val res = typeCheckErrors("""NegativeInt(1)""").head
        assertTrue(res.message contains "negative")
      }
    }

    suiteAll("PositiveLong") {
      test("valid positive") {
        val res = PositiveLong(1L)
        assertTrue(res == 1L)
      }

      test("invalid zero") {
        val res = typeCheckErrors("""PositiveLong(0L)""").head
        assertTrue(res.message contains "positive")
      }
    }

    suiteAll("Percentage") {
      test("valid 0") {
        val res = Percentage(0.0)
        assertTrue(res == 0.0)
      }

      test("valid 50") {
        val res = Percentage(50.0)
        assertTrue(res == 50.0)
      }

      test("valid 100") {
        val res = Percentage(100.0)
        assertTrue(res == 100.0)
      }

      test("invalid negative") {
        val res = typeCheckErrors("""Percentage(-1.0)""").head
        assertTrue(res.message contains "between 0 and 100")
      }

      test("invalid over 100") {
        val res = typeCheckErrors("""Percentage(101.0)""").head
        assertTrue(res.message contains "between 0 and 100")
      }
    }

    suiteAll("UnitInterval") {
      test("valid 0") {
        val res = UnitInterval(0.0)
        assertTrue(res == 0.0)
      }

      test("valid 0.5") {
        val res = UnitInterval(0.5)
        assertTrue(res == 0.5)
      }

      test("valid 1") {
        val res = UnitInterval(1.0)
        assertTrue(res == 1.0)
      }

      test("invalid negative") {
        val res = typeCheckErrors("""UnitInterval(-0.1)""").head
        assertTrue(res.message contains "between 0.0 and 1.0")
      }

      test("invalid over 1") {
        val res = typeCheckErrors("""UnitInterval(1.1)""").head
        assertTrue(res.message contains "between 0.0 and 1.0")
      }
    }

    suiteAll("PortNumber") {
      test("valid 0") {
        val res = PortNumber(0)
        assertTrue(res == 0)
      }

      test("valid 80") {
        val res = PortNumber(80)
        assertTrue(res == 80)
      }

      test("valid 65535") {
        val res = PortNumber(65535)
        assertTrue(res == 65535)
      }

      test("invalid negative") {
        val res = typeCheckErrors("""PortNumber(-1)""").head
        assertTrue(res.message contains "port number")
      }

      test("invalid over 65535") {
        val res = typeCheckErrors("""PortNumber(65536)""").head
        assertTrue(res.message contains "port number")
      }
    }

    suiteAll("makeOrThrow on common types") {
      test("PositiveInt.makeOrThrow success") {
        val res = PositiveInt.makeOrThrow(42)
        assertTrue(res == 42)
      }

      test("PositiveInt.makeOrThrow failure") {
        val error = scala.util.Try(PositiveInt.makeOrThrow(-1)).failed.get
        assertTrue(error.getMessage == "Must be positive")
      }

      test("EmailString.makeOrThrow success") {
        val res = EmailString.makeOrThrow("test@example.com")
        assertTrue(res == "test@example.com")
      }

      test("EmailString.makeOrThrow failure") {
        val error = scala.util.Try(EmailString.makeOrThrow("invalid")).failed.get
        assertTrue(error.getMessage == "Must be a valid email address")
      }
    }
  }
