package neotype.doobie

import _root_.doobie.*
import _root_.doobie.implicits.*
import _root_.doobie.postgres.implicits.*
import _root_.doobie.util.transactor.Transactor
import cats.Show
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import neotype.Newtype
import neotype.Subtype
import neotype.test.definitions.*
import zio.*
import zio.test.*

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
        val result =
          scala.util.Try(sql"SELECT ''".query[ValidatedNewtype].unique.transact(transactor).unsafeRunSync())
        assertTrue(result.isFailure)
      },
      test("simple newtype success") {
        val result = sql"SELECT 42".query[SimpleNewtype].unique.transact(transactor).unsafeRunSync()
        assertTrue(result == SimpleNewtype(42))
      },
      test("array success") {
        val result =
          sql"SELECT ARRAY['Hello', 'World']".query[Array[ValidatedNewtype]].unique.transact(transactor).unsafeRunSync()
        assertTrue(result.toSet == Set(ValidatedNewtype("Hello"), ValidatedNewtype("World")))
      },
      test("array fail") {
        val result =
          scala.util.Try(
            sql"SELECT ARRAY['Hello', 'World', '']"
              .query[Array[ValidatedNewtype]]
              .unique
              .transact(transactor)
              .unsafeRunSync()
          )
        assertTrue(result.toEither.is(_.left).getMessage.contains("String must not be empty"))
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
      },
      test("array subtype success") {
        val result =
          sql"SELECT ARRAY['Hello There Forever', 'Nice Day Time']"
            .query[Array[ValidatedSubtype]]
            .unique
            .transact(transactor)
            .unsafeRunSync()
        assertTrue(result.toSet == Set(ValidatedSubtype("Hello There Forever"), ValidatedSubtype("Nice Day Time")))
      },
      test("array subtype fail") {
        val result = scala.util.Try(
          sql"SELECT ARRAY['Hello There Forever', 'Nice Day Time', '2 SHORT']"
            .query[Array[ValidatedSubtype]]
            .unique
            .transact(transactor)
            .unsafeRunSync()
        )
        assertTrue(result.toEither.is(_.left).getMessage.contains("2 SHORT"))
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
