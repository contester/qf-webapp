package models

import com.typesafe.config.ConfigFactory
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Clarification, Extrainfo, MaxSeen, Message2, SlickModel}
import play.api.libs.json.Json
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The per-round tables written and read through the Slick model on the teacher-web schema: every insert carries the
  * round's season, and the reads join back. Skipped unless QF_TEST_URL is set (see ContestRoundsDbSpec); the rows
  * it adds are deleted afterwards.
  */
class ContestDataDbSpec extends AnyFunSuite with BeforeAndAfterAll {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val url = sys.env.get("QF_TEST_URL")

  private lazy val dbConfig = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("QF_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("QF_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))

  private lazy val db = dbConfig.db

  private def await[T](f: Future[T]) = Await.result(f, 60.seconds)

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  private lazy val round: Int = await(db.run(sql"select id from contest_rounds where testing_system_id = 62042".as[Int].head))
  private lazy val season: Int = await(db.run(sql"select season_id from contest_rounds where id = $round".as[Int].head))
  private lazy val team: Int = await(db.run(sql"select participant_id from round_credentials where login = 'team1_1' and round_id = $round".as[Int].head))
  private lazy val language: Int = await(db.run(sql"select id from season_languages where module_id = 'g++'".as[Int].head))

  private def cleanup(): Unit = {
    await(db.run(DBIO.seq(
      sqlu"delete from results where testing_id in (select t.id from testings t join submits s on s.id = t.submit_id where s.round_id = $round)",
      sqlu"delete from testing_groups where testing_id in (select t.id from testings t join submits s on s.id = t.submit_id where s.round_id = $round)",
      sqlu"update submits set testing_id = null where round_id = $round",
      sqlu"delete from testings where submit_id in (select id from submits where round_id = $round)",
      sqlu"delete from submits where round_id = $round",
      sqlu"delete from problems where round_id = $round",
      sqlu"delete from custom_tests where round_id = $round",
      sqlu"delete from clarifications where round_id = $round",
      sqlu"delete from clarification_requests where round_id = $round",
      sqlu"delete from clarifications_seen where round_id = $round",
      sqlu"delete from messages where round_id = $round",
      sqlu"delete from extra_info where round_id = $round"
    )))
  }

  override protected def afterAll(): Unit = {
    if (url.isDefined) cleanup()
    super.afterAll()
  }

  test("a team logs in with its round credential and gets its round and team") {
    assume(url.isDefined)
    val lt = await(Users.authenticate(db, "team1_1", "482913")).getOrElse(fail("no login"))
    assert(lt.contest.id == round && lt.contest.seasonId == season)
    assert(lt.team.id == team && lt.team.name == "Participant 1" && lt.team.school == "Test School")
    assert(await(Users.authenticate(db, "team1_1", "wrong")).isEmpty)
    assert(await(Users.resolve(db, lt.sessionKey)).map(_.team.id).contains(team))
  }

  test("clarifications, requests, seen marks, messages and extra pages are written with the round's season") {
    assume(url.isDefined)
    val cl = await(db.run(SlickModel.insertClarification(Clarification(None, round, "A", "text", DateTime.now, hidden = false))))
    assert(cl.id.isDefined)
    assert(await(db.run(SlickModel.getClarificationsForContest(round).result)).map(_.id) == Seq(cl.id))
    assert(await(db.run(sql"select season_id from clarifications where id = ${cl.id.get}".as[Int].head)) == season)

    val reqId = await(db.run((SlickModel.clarificationRequests.map(x => (x.seasonId, x.contest, x.team, x.problem, x.request))
      returning SlickModel.clarificationRequests.map(_.id)) += ((season, round, team, "A", "why?"))))
    assert(await(ClarificationModel.getTeamClarificationReqs(db, round, team)).map(_.id) == Seq(reqId))

    val t1 = new DateTime(2026, 11, 1, 10, 0)
    await(db.run(SlickModel.upsertMaxSeen(MaxSeen(round, team, t1))))
    await(db.run(SlickModel.upsertMaxSeen(MaxSeen(round, team, t1.plusMinutes(5)))))
    val seen = await(db.run(SlickModel.clrSeen2.filter(_.contest === round).result))
    assert(seen.size == 1 && seen.head.timestamp.getMillis == t1.plusMinutes(5).getMillis)

    val m = await(db.run(SlickModel.addMessage(Message2(None, round, team, "custom", Json.obj("x" -> 1), seen = false))))
    assert(await(db.run(SlickModel.messages2Unseen.result)).exists(x => x.id == m.id && x.data == Json.obj("x" -> 1)))
    await(db.run(SlickModel.messageSeenByID(m.id.get).update(true)))
    assert(!await(db.run(SlickModel.messages2Unseen.result)).exists(_.id == m.id))

    assert(await(AdminExtraInfo.save(db, round, None, Extrainfo(0, 1, "Rules", "Be nice"))) == AdminExtraInfo.Saved)
    assert(await(AdminExtraInfo.list(db, round)).map(_.heading) == Seq("Rules"))
  }

  test("a submit with a testing and results reads back through the submit, status and rejudge queries") {
    assume(url.isDefined)
    await(db.run(sqlu"""insert into problems (season_id, round_id, problem_id, name, tests, max_points)
                        values ($season, $round, 'A', 'Alpha', 2, 20)"""))
    val (id, arrived) = await(db.run((SlickModel.submits.map(x => (x.seasonId, x.contest, x.team, x.problem, x.language, x.source))
      returning SlickModel.submits.map(x => (x.id, x.arrived))) += ((season, round, team, "A", language, "int main(){}".getBytes))))
    assert(arrived.isBefore(DateTime.now.plusSeconds(1)))
    val testing = await(db.run(sql"""insert into testings (submit_id, problem_revision, finished_at)
                                     values ($id, 7, now()) returning id""".as[Long].head))
    await(db.run(DBIO.seq(
      sqlu"""insert into results (testing_id, test_no, group_name, result_code, points, time_ms, memory_bytes)
             values ($testing, 0, null, 1, 0, 0, 0), ($testing, 1, '1', 10, 10, 15, 2048), ($testing, 2, '1', 13, 0, 20, 4096)""",
      sqlu"""insert into testing_groups (testing_id, group_name, policy, points_earned, points_max, passed, skipped)
             values ($testing, '1', 'each-test', 10, 20, false, false)""",
      sqlu"update submits set tested = true, compiled = true, passed = 1, taken = 2, points = 10, testing_id = $testing where id = $id"
    )))

    val subs = await(db.run(Submits.getContestTeamSubmits(round, team))).map(Submits.upliftSub)
    assert(subs.map(_.submitId.id.toLong) == Seq(id))
    val s = subs.head
    assert(s.submitId.ext == "g++" && s.finished && s.compiled && s.passed == 1 && s.taken == 2 && s.testingId.contains(testing.toInt))
    assert(s.submitId.arrived.seconds > 0)

    val details = await(Submits.loadSubmitDetails(db, testing.toInt))
    assert(details.map(_.test) == Seq(0, 1, 2))
    val described = await(Submits.teamProblemSubmits(db, round, team, "A"))
    assert(described.map(_.result.message) == Seq("10 из 20"))
    assert(await(Submits.getSubmitById(db, id.toInt)).map(_.sourceStr).contains("int main(){}"))

    val rows = await(db.run(SlickModel.results0ByTesting(testing).result))
    assert(rows.map(r => (r.testID, r.groupName, r.points)) == Seq((0, None, 0.0), (1, Some("1"), 10.0), (2, Some("1"), 0.0)))
    assert(await(db.run(SlickModel.testingGroups.filter(_.testingId === testing).result)).map(_.policy) == Seq("each-test"))
    assert(await(db.run(SlickModel.problems.filter(_.contestID === round).result)).map(p => (p.id, p.maxPoints)) == Seq(("A", 20.0)))

    val page = await(AdminSubmits.page(db, round, None, None, None))
    assert(page.rows.map(r => (r.submit.submitId.id.toLong, r.runs, r.result.message)) == Seq((id, 1, "10 из 20")))
    assert(page.rows.head.stats.timeMs.underlying == 20)
    val runs = await(AdminSubmits.runHistory(db, id, Some(testing)))
    assert(runs.map(r => (r.testingId, r.problemURL, r.current)) == Seq((testing, "revision 7", true)))
    val status = await(AdminSubmits.revisionStatus(db, round))
    assert(status.map(x => (x.problem, x.latestURL, x.atLatest, x.total)) == Seq(("A", Some("revision 7"), 1, 1)))
    assert(await(AdminSubmits.rejudgePlan(db, round, "A", "revision 7")).current == 1)
    assert(await(AdminSubmits.allSubmitIds(db, round, "A")) == Seq(id))
  }

  test("a custom test reads back through the team and admin queries") {
    assume(url.isDefined)
    val id = await(db.run((SlickModel.customTests.map(x => (x.seasonId, x.contest, x.team, x.language, x.source, x.input))
      returning SlickModel.customTests.map(_.id)) += ((season, round, team, language, "src".getBytes, "in".getBytes))))
    val page = await(AdminCustomTests.page(db, round, Some(team), None, None))
    assert(page.rows.map(r => (r.id, r.language, r.running)) == Seq((id, "GNU C++", true)))
    await(db.run(sqlu"update custom_tests set tested = true, finished_at = now(), output = 'out'::bytea where id = $id"))
    val e = await(AdminCustomTests.get(db, round, id)).getOrElse(fail("no custom test"))
    assert(e.finishTime.isDefined && e.outputStr == "out" && e.languageName == "GNU C++")
    val copy = await(AdminCustomTests.rerun(db, round, id)).getOrElse(fail("no copy"))
    assert(copy != id)
    assert(await(AdminCustomTests.get(db, round, copy)).exists(c => c.finishTime.isEmpty && c.sourceStr == "src"))
    assert(await(AdminCustomTests.get(db, round, id)).exists(_.finishTime.isDefined))
  }
}
