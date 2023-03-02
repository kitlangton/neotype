package neotype.zioquill

import io.getquill.*
import io.getquill.jdbczio.Quill
import neotype.zioquill.Queries.context
import neotype.{Newtype, Subtype}
import zio.test.*

type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  inline def validate(value: String): Boolean =
    value.nonEmpty

type SubtypeLongString = SubtypeLongString.Type
given SubtypeLongString: Subtype[String] with
  inline def validate(value: String): Boolean =
    value.length > 10

final case class Person(name: NonEmptyString, age: Int)

object Queries:
  val h2DataSource = new org.h2.jdbcx.JdbcDataSource
  h2DataSource.setURL("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1")
  h2DataSource.setUser("sa")
  h2DataSource.setPassword("")
  val context = new H2JdbcContext(SnakeCase, h2DataSource)
  import context.*

  val createTable = run {
    sql"""
CREATE TABLE IF NOT EXISTS Person (
    name VARCHAR(255) NOT NULL,
    age INT NOT NULL);

INSERT INTO Person (name, age) VALUES ('Jimmy Jazz', 1);
INSERT INTO Person (name, age) VALUES ('Barry Blues', 2);
       """.as[Update[Unit]]
  }

  def insertEmptyNamed = run {
    sql"""
INSERT INTO Person (name, age) VALUES ('', 3);
       """.as[Update[Unit]]
  }

  def getPersons = run(query[Person])

object ZioQuillSpec extends ZIOSpecDefault:
  def spec = suite("ZioQuillSpec") {
    suite("NonEmptyString")(
      test("success") {
        val result = Queries.getPersons
        assertTrue(
          result == List( //
            Person(NonEmptyString("Jimmy Jazz"), 1),
            Person(NonEmptyString("Barry Blues"), 2)
          )
        )
      },
      test("fail") {
        Queries.insertEmptyNamed
        val result = scala.util.Try(Queries.getPersons)
        assertTrue(
          result
            .asInstanceOf[scala.util.Failure[?]]
            .exception
            .getMessage == "Failed to create newtype: Validation Failed"
        )
      }
    ) @@ TestAspect.sequential
  }
