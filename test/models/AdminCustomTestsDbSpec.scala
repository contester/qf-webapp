package models

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/**
  * The admin custom tests list must agree with the full rows (read-only; skipped unless ADMINS_TEST_URL is set,
  * same variables as AdminSubmitsDbSpec). Expects a contest with a few custom tests.
  */
class AdminCustomTestsDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val url = sys.env.get("ADMINS_TEST_URL")

  private lazy val dbConfig = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("ADMINS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("ADMINS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))

  private def await[T](f: Future[T]) = Await.result(f, 60.seconds)
  private lazy val db = dbConfig.db

  private def contestWithTests: Option[Int] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    await(db.run(sql"select contest from custom_test group by contest order by count(*) desc limit 1".as[Int])).headOption
  }

  test("list rows match the full rows, in every page size") {
    assume(url.isDefined)
    val contest = contestWithTests
    assume(contest.isDefined)
    val c = contest.get
    val all = await(AdminCustomTests.page(db, c, None, None, None, size = 100000))
    assert(all.rows.map(_.id) == all.rows.map(_.id).sorted.reverse)
    all.rows.take(20).foreach { row =>
      val full = await(AdminCustomTests.get(db, c, row.id)).get
      assert(row.sourceSize == full.source.length && row.inputSize == full.input.length && row.outputSize == full.output.length)
      assert(row.resultCode == full.resultCode && row.team == full.team && row.language == full.languageName)
      assert(row.running == full.finishTime.isEmpty)
    }
    assert(await(AdminCustomTests.get(db, c + 100000, all.rows.head.id)).isEmpty, "other contest's test must not leak")

    var seen = Seq[Long]()
    var page = await(AdminCustomTests.page(db, c, None, None, None, size = 3))
    seen ++= page.rows.map(_.id)
    var guard = 0
    while (page.older.isDefined && guard < 1000) {
      guard += 1
      page = await(AdminCustomTests.page(db, c, None, page.older, None, size = 3))
      seen ++= page.rows.map(_.id)
    }
    assert(seen == all.rows.map(_.id))
    // and back up again through `after`
    var p = await(AdminCustomTests.page(db, c, None, None, Some(all.rows.last.id - 1), size = 3))
    assert(p.rows.map(_.id) == all.rows.takeRight(3).map(_.id))
  }
}
