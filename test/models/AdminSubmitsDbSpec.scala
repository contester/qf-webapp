package models

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/**
  * The paginated admin status page must show what the old load-everything pipeline shows
  * (skipped unless ADMINS_TEST_URL is set, same database as AdminsDbSpec but with the contest schema and data):
  *   ADMINS_TEST_URL=jdbc:postgresql://127.0.0.1:55432/contest ADMINS_TEST_USER=stingray ADMINS_TEST_PASSWORD=s
  * Expects contests with submits and at least one rejudged submit.
  */
class AdminSubmitsDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val url = sys.env.get("ADMINS_TEST_URL")

  private lazy val dbConfig = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("ADMINS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("ADMINS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))

  private def await[T](f: Future[T]) = Await.result(f, 60.seconds)
  private lazy val db = dbConfig.db

  private def oldPipeline(contest: Int, team: Option[Int] = None) = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    // the old page scored with all of the team's submits, so only filter after annotating
    await(Submits.groupAndAnnotate(db, false, await(db.run(Submits.getContestSubmits(contest))).map(Submits.upliftSub(_))))
      .filter(f => team.forall(_ == f.submit.submitId.teamId)).sortBy(-_.submit.submitId.id)
  }

  private def allPages(contest: Int, team: Option[Int], size: Int) = {
    var rows = Seq[AdminSubmitRow]()
    var page = await(AdminSubmits.page(db, contest, team, None, None, canSeeAll = true, size))
    rows ++= page.rows
    var guard = 0
    while (page.older.isDefined && guard < 100) {
      guard += 1
      page = await(AdminSubmits.page(db, contest, team, page.older, None, canSeeAll = true, size))
      rows ++= page.rows
    }
    rows
  }

  for (contest <- Seq(14, 12, 13)) test(s"contest $contest: pages agree with the old pipeline") {
    assume(url.isDefined)
    val old = oldPipeline(contest)
    val one = await(AdminSubmits.page(db, contest, None, None, None, canSeeAll = true))
    assert(one.older.isEmpty && one.newer.isEmpty)
    assert(one.rows.map(_.submit.submitId.id) == old.map(_.submit.submitId.id))
    one.rows.zip(old).foreach { case (n, o) =>
      assert(n.result.message == o.result.message, s"submit ${n.submit.submitId.id}")
      assert(n.score.map(_.toString) == o.score.map(_.toString), s"score of ${n.submit.submitId.id}")
      assert(n.stats == o.stats, s"stats of ${n.submit.submitId.id}")
    }
    // small pages walk the same rows, in the same order, with the same scores
    val walked = allPages(contest, None, 2)
    assert(walked.map(_.submit.submitId.id) == one.rows.map(_.submit.submitId.id))
    assert(walked.map(_.score.map(_.toString)) == one.rows.map(_.score.map(_.toString)))
  }

  test("newer pages mirror the older ones") {
    assume(url.isDefined)
    val first = await(AdminSubmits.page(db, 14, None, None, None, canSeeAll = true, 4))
    assert(first.newer.isEmpty && first.older.isDefined)
    val second = await(AdminSubmits.page(db, 14, None, first.older, None, canSeeAll = true, 4))
    assert(second.newer.isDefined)
    val back = await(AdminSubmits.page(db, 14, None, None, second.newer, canSeeAll = true, 4))
    assert(back.rows.map(_.submit.submitId.id) == first.rows.map(_.submit.submitId.id))
    assert(back.newer.isEmpty && back.older.isDefined)
  }

  test("team filter keeps the scores of the full history") {
    assume(url.isDefined)
    val team = await(AdminSubmits.page(db, 14, Some(8), None, None, canSeeAll = true)).rows
    assert(team.nonEmpty && team.forall(_.submit.submitId.teamId == 8))
    assert(await(AdminSubmits.page(db, 14, Some(99999), None, None, canSeeAll = true)).rows.isEmpty)
    val old = oldPipeline(14, Some(8))
    assert(team.map(_.score.map(_.toString)) == old.map(_.score.map(_.toString)))
  }

  test("rejudged submits show their run count and history") {
    assume(url.isDefined)
    val rows = await(AdminSubmits.page(db, 14, None, None, None, canSeeAll = true)).rows
    assert(rows.find(_.submit.submitId.id == 4).get.runs == 2)
    assert(rows.find(_.submit.submitId.id == 1).get.runs == 1)
    val runs = await(AdminSubmits.runHistory(db, 4, Some(10)))
    assert(runs.map(_.testingId) == runs.map(_.testingId).sorted.reverse && runs.size == 2)
    assert(runs.count(_.current) == 1 && runs.find(_.current).get.testingId == 10)
  }
}
