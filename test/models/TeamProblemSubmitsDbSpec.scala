package models

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/**
  * The rows sent with a finished testing (two queries per team and problem) must describe every attempt exactly as
  * the team's submit list does (skipped unless ADMINS_TEST_URL is set, any copy of a contest database will do):
  *   podman run -d --name qf-test-pg -e POSTGRES_USER=stingray -e POSTGRES_PASSWORD=s -e POSTGRES_DB=contest \
  *     -p 127.0.0.1:55432:5432 docker.io/library/postgres:17
  *   podman exec -i qf-test-pg psql -U stingray -d contest < contest_dump.sql
  *   ADMINS_TEST_URL=jdbc:postgresql://127.0.0.1:55432/contest ADMINS_TEST_USER=stingray ADMINS_TEST_PASSWORD=s sbt test
  */
class TeamProblemSubmitsDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val url = sys.env.get("ADMINS_TEST_URL")

  private lazy val dbConfig = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("ADMINS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("ADMINS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))

  private def await[T](f: Future[T]) = Await.result(f, 120.seconds)
  private lazy val db = dbConfig.db

  private def summary(fs: FullyDescribedSubmit) =
    (fs.submit.submitId.id, fs.index, fs.score.map(_.toString), fs.result.success, fs.result.message, fs.stats)

  for (schoolMode <- Seq(false, true)) test(s"every team and problem agrees with the full pipeline (schoolMode = $schoolMode)") {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    assume(url.isDefined)
    val contests = await(db.run(org.stingray.contester.dbmodel.SlickModel.allContests.result)).map(_.id)
    var groups = 0
    for (contest <- contests) {
      val all = await(db.run(Submits.getContestSubmits(contest))).map(Submits.upliftSub(_))
      val old = await(Submits.groupAndAnnotate(db, schoolMode, all))
        .groupBy(x => (x.submit.submitId.teamId, x.submit.submitId.problem.id))
      for (((team, problem), expected) <- old) {
        val got = await(Submits.teamProblemSubmits(db, schoolMode, contest, team, problem))
        assert(got.map(summary) == expected.sortBy(_.index).map(summary), s"contest $contest team $team problem $problem")
        groups += 1
      }
    }
    assert(groups > 0, "the database has no submits to compare")
  }

  test("a team and problem without submits have no rows") {
    assume(url.isDefined)
    assert(await(Submits.teamProblemSubmits(db, schoolMode = false, 14, 99999, "A")).isEmpty)
  }
}
