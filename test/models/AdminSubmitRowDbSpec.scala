package models

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The row the live status page pushes for one submit (AdminSubmits.row) must be the row the page itself shows for
  * it, in every contest of the database (skipped unless ADMINS_TEST_URL is set, like AdminSubmitsDbSpec; any data
  * will do):
  *   ADMINS_TEST_URL=jdbc:postgresql://127.0.0.1:54329/contest ADMINS_TEST_USER=postgres ADMINS_TEST_PASSWORD=x
  */
class AdminSubmitRowDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val url = sys.env.get("ADMINS_TEST_URL")

  private lazy val dbConfig = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("ADMINS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("ADMINS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))

  private def await[T](f: Future[T]) = Await.result(f, 120.seconds)
  private lazy val db = dbConfig.db

  private lazy val contests: Seq[Int] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    await(models.Rounds.currentRounds(db)).map(_.id)
  }

  private def whole(contest: Int) =
    await(AdminSubmits.page(db, contest, None, None, None, size = 100000)).rows

  test("every submit's row is the row the page shows for it") {
    assume(url.isDefined)
    var checked = 0
    contests.foreach { c =>
      whole(c).foreach { shown =>
        val id = shown.submit.submitId.id
        val one = await(AdminSubmits.row(db, id)).getOrElse(fail(s"no row for submit $id of contest $c"))
        assert(one.submit.submitId.contestId == c && one.submit.submitId.teamId == shown.submit.submitId.teamId, s"submit $id")
        assert(one.result.message == shown.result.message, s"verdict of $id")
        assert(one.stats == shown.stats, s"stats of $id")
        assert(one.runs == shown.runs, s"runs of $id")
        assert(one.submit.submitId.arrived == shown.submit.submitId.arrived, s"arrival of $id")
        checked += 1
      }
    }
    assert(checked > 0, "the database has no submits")
    info(s"$checked submits in ${contests.size} contests")
  }

  test("a submit that is not there has no row") {
    assume(url.isDefined)
    assert(await(AdminSubmits.row(db, Int.MaxValue)).isEmpty)
  }
}
