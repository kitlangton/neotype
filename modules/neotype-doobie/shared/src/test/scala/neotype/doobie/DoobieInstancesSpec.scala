package neotype.doobie

import _root_.doobie.*
import _root_.doobie.implicits.{*, given}
import _root_.doobie.util.transactor.Transactor
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import neotype.Newtype
import neotype.Subtype
import neotype.test.definitions.*
import zio.*
import zio.test.* // Added this import

object DoobieInstancesSpec extends ZIOSpecDefault:

  val transactor = Transactor.fromDriverManager[IO](
    driver = "org.h2.Driver",                   // driver classname
    url = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", // connect URL (memory database)
    logHandler = None                           // log handler
  )

  def spec = suite("DoobieInstancesSpec")(
    suite("Newtype")(
      test("success") {
        val result = sql"SELECT 'Hello'".query[ValidatedNewtype].unique.transact(transactor).unsafeRunSync()
        assertTrue(result == ValidatedNewtype("Hello"))
      },
      test("fail") {
        val result = scala.util.Try(sql"SELECT ''".query[ValidatedNewtype].unique.transact(transactor).unsafeRunSync())
        assertTrue(result.isFailure)
      },
      test("simple newtype success") {
        val result = sql"SELECT 42".query[SimpleNewtype].unique.transact(transactor).unsafeRunSync()
        assertTrue(result == SimpleNewtype(42))
      }
    ),
    suite("Subtype")(
      test("success") {
        val result = sql"SELECT 'Hello World!'".query[ValidatedSubtype].unique.transact(transactor).unsafeRunSync()
        assertTrue(result == ValidatedSubtype("Hello World!"))
      },
      test("fail") {
        val result = scala.util.Try(sql"SELECT ''".query[ValidatedSubtype].unique.transact(transactor).unsafeRunSync())
        assertTrue(result.isFailure)
      },
      test("simple subtype success") {
        val result = sql"SELECT 100".query[SimpleSubtype].unique.transact(transactor).unsafeRunSync()
        assertTrue(result == SimpleSubtype(100))
      }
    ),
    suite("Composite")(
      test("composite success") {
        val result = sql"SELECT 'Hello', 42, 'Hello World!', 100"
          .query[Composite]
          .unique
          .transact(transactor)
          .unsafeRunSync()
        assertTrue(
          result == Composite(
            ValidatedNewtype("Hello"),
            SimpleNewtype(42),
            ValidatedSubtype("Hello World!"),
            SimpleSubtype(100)
          )
        )
      },
      test("composite fail") {
        val result = scala.util.Try(
          sql"SELECT '', 42, 'Short', 100"
            .query[Composite]
            .unique
            .transact(transactor)
            .unsafeRunSync()
        )
        assertTrue(result.isFailure)
      }
    )
  )
