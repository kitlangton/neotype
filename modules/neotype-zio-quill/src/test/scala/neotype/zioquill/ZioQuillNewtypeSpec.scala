package neotype.zioquill

import io.getquill.*
import neotype.Newtype
import neotype.Subtype
import neotype.test.definitions.*
import zio.test.*

object Queries:
  val h2DataSource = new org.h2.jdbcx.JdbcDataSource
  h2DataSource.setURL("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1")
  h2DataSource.setUser("sa")
  h2DataSource.setPassword("")
  val context = new H2JdbcContext(SnakeCase, h2DataSource)
  import context.*

  val createTable = run {
    sql"""
      CREATE TABLE composite (
        newtype VARCHAR(255),
        simple_newtype INT,
        subtype VARCHAR(255),
        simple_subtype INT
      );

      INSERT INTO composite (newtype, simple_newtype, subtype, simple_subtype)
      VALUES ('Hello', 123, 'Hello World!', 123456),
             ('Cool', 321, 'Hello World!', 123456);
         """.as[Update[Unit]]
  }

  def insertInvalidRow = run {
    sql"""
    INSERT INTO composite (newtype, simple_newtype, subtype, simple_subtype)
    VALUES ('', 123, 'Hello', 123456);
       """.as[Update[Unit]]
  }

  def getValues = run(query[Composite])

object ZioQuillSpec extends ZIOSpecDefault:
  def spec = suite("ZioQuillSpec") {
    suite("NonEmptyString")(
      test("success") {
        val result = Queries.getValues
        assertTrue(
          result == List( //
            Composite(
              ValidatedNewtype("Hello"),
              SimpleNewtype(123),
              ValidatedSubtype("Hello World!"),
              SimpleSubtype(123456)
            ),
            Composite(
              ValidatedNewtype("Cool"),
              SimpleNewtype(321),
              ValidatedSubtype("Hello World!"),
              SimpleSubtype(123456)
            )
          )
        )
      },
      test("fail") {
        Queries.insertInvalidRow
        val result = scala.util.Try(Queries.getValues)
        assertTrue(
          result
            .asInstanceOf[scala.util.Failure[?]]
            .exception
            .getMessage == "Failed to create newtype: String must not be empty"
        )
      }
    ) @@ TestAspect.sequential
  }
