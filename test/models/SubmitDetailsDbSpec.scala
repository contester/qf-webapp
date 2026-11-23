package models

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Memory, TimeMs}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The per-test table of a submit page, loaded the way the controllers load it (Submits.loadTestRows). Skipped unless
  * QF_TEST_URL is set (see SubmitInsertDbSpec). Uses a round and participant of its own and deletes them afterwards.
  */
class SubmitDetailsDbSpec extends AnyFunSuite {
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

  private def exec(sql: DBIO[Int]): Int = await(db.run(sql))

  private val roundName = "Details spec round"
  private val kidName = "Details Spec Kid"

  private def cleanup(): Unit = {
    val rounds = "select id from contest_rounds where name = 'Details spec round'"
    exec(sqlu"update submits set testing_id = null where round_id in (select id from contest_rounds where name = 'Details spec round')")
    exec(sqlu"delete from results where testing_id in (select t.id from testings t join submits s on s.id = t.submit_id join contest_rounds r on r.id = s.round_id where r.name = 'Details spec round')")
    exec(sqlu"delete from testing_groups where testing_id in (select t.id from testings t join submits s on s.id = t.submit_id join contest_rounds r on r.id = s.round_id where r.name = 'Details spec round')")
    exec(sqlu"delete from testings where submit_id in (select s.id from submits s join contest_rounds r on r.id = s.round_id where r.name = 'Details spec round')")
    exec(sqlu"delete from submits where round_id in (select id from contest_rounds where name = 'Details spec round')")
    exec(sqlu"delete from problems where round_id in (select id from contest_rounds where name = 'Details spec round')")
    exec(sqlu"delete from round_credentials where round_id in (select id from contest_rounds where name = 'Details spec round')")
    exec(sqlu"delete from participants where full_name = 'Details Spec Kid'")
    exec(sqlu"delete from contest_rounds where name = 'Details spec round'")
  }

  test("the loaded results and groups of the current testing make the per-test table") {
    assume(url.isDefined)
    val seedRound = await(db.run(sql"select id from contest_rounds where testing_system_id = 62042".as[Int].head))
    val season = await(db.run(sql"select season_id from contest_rounds where id = $seedRound".as[Int].head))
    val language = await(db.run(sql"select id from season_languages where season_id = $season and module_id = 'g++'".as[Int].head))
    cleanup()
    try {
      exec(sqlu"""insert into contest_rounds (contest_id, season_id, round_seq, season_seq, name, starts_at, ends_at, is_finished)
                  select c.id, c.season_id, 81, 81, 'Details spec round', now(), now() + interval '2 hours', false
                    from contests c where c.season_id = $season""")
      val round = await(db.run(sql"select id from contest_rounds where name = 'Details spec round'".as[Int].head))
      exec(sqlu"""insert into participants (season_id, teacher_id, season_seq, full_name, school_id, grade, contest_id)
                  select p.season_id, p.teacher_id, 96, 'Details Spec Kid', p.school_id, 9, p.contest_id
                    from participants p where p.id = (select participant_id from round_credentials where round_id = $seedRound and login = 'team1_1')""")
      val kid = await(db.run(sql"select id from participants where full_name = 'Details Spec Kid'".as[Int].head))
      exec(sqlu"""insert into round_credentials (round_id, contest_id, participant_id, login, password)
                  select r.id, r.contest_id, $kid, 'details_kid', '777001' from contest_rounds r where r.id = $round""")
      exec(sqlu"insert into problems (season_id, round_id, problem_id, name, tests, max_points) values ($season, $round, 'D', 'Dee', 5, 8)")
      val id = await(db.run(sql"""insert into submits (season_id, round_id, participant_id, problem_id, language_id, source)
                                  values ($season, $round, $kid, 'D', $language, 'x'::bytea) returning id""".as[Long].head))
      // an older testing, which the page must not mix in, and the current one
      val old = await(db.run(sql"insert into testings (submit_id, finished_at) values ($id, now()) returning id".as[Long].head))
      val cur = await(db.run(sql"insert into testings (submit_id, finished_at) values ($id, now()) returning id".as[Long].head))
      exec(sqlu"""insert into results (testing_id, test_no, group_name, result_code, points, time_ms, memory_bytes) values
                  ($old, 1, '1', 13, 0, 1, 1024)""")
      exec(sqlu"""insert into results (testing_id, test_no, group_name, result_code, points, time_ms, memory_bytes) values
                  ($cur, 0, null, 1, 0, 0, 0),
                  ($cur, 3, '2', 10, 0, 30, 3072),
                  ($cur, 1, '1', 10, 2.5, 10, 1024),
                  ($cur, 2, '1', 13, 0, 20, 2048),
                  ($cur, 4, '2', 13, 0, 40, 4096),
                  ($cur, 5, '2', 18, 0, 0, 0)""")
      exec(sqlu"""insert into testing_groups (testing_id, group_name, policy, points_earned, points_max, passed, skipped) values
                  ($cur, '1', 'each-test', 2.5, 5, false, false), ($cur, '2', 'complete-group', 0, 3, false, false),
                  ($old, '1', 'each-test', 0, 5, false, false)""")
      exec(sqlu"update submits set tested = true, compiled = true, passed = 1, taken = 5, points = 2.5, testing_id = $cur where id = $id")

      val rows = await(Submits.loadTestRows(db, cur))
      assert(rows.map(_.testNo) == Seq(1, 2, 3, 4, 5))
      assert(rows.map(_.pointsText) == Seq("2.5 (тест пройден)", "0 (тест не пройден)", "тест пройден", "тест не пройден", "не запускался"))
      assert(rows.map(_.group) == Seq(Some("1"), Some("1"), Some("2"), Some("2"), Some("2")))
      assert(rows.map(_.policy) == Seq("each-test", "each-test", "complete-group", "complete-group", "complete-group").map(Some(_)))
      assert(rows.map(_.verdict) == Seq("Ok", "Wrong answer", "Ok", "Wrong answer", "не запускался"))
      assert(rows.head.timeMs == TimeMs(10) && rows.head.memoryBytes == Memory(1024))

      // the submit as the list shows it: points of max
      val sub = Submits.upliftSub(await(db.run(SlickModel0.byId(id))))
      assert(SubmitResult.fromSubmit(sub).message == "Баллы: 2.5 из 8")
      // the older testing keeps its own rows
      assert(await(Submits.loadTestRows(db, old)).map(_.pointsText) == Seq("0 (тест не пройден)"))
    } finally cleanup()
  }

  private object SlickModel0 {
    def byId(id: Long) = org.stingray.contester.dbmodel.SlickModel.submitsUnsorted.filter(_.id === id).result.head
  }
}
