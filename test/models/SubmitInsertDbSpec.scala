package models

import com.typesafe.config.ConfigFactory
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, Team}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import java.sql.DriverManager
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Submits.insert against the teacher-web schema. Skipped unless QF_TEST_URL is set; expects the database migrated
  * by `teacherweb migrate` and seeded with teacher-web's doc/testdata/seed.sql:
  *   QF_TEST_URL=jdbc:postgresql://127.0.0.1:55433/contester QF_TEST_USER=postgres QF_TEST_PASSWORD=x
  * Each test sets the round times it needs and puts everything back.
  */
class SubmitInsertDbSpec extends AnyFunSuite {
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

  // The seed round is only read; the specs here use a round, participants and a problem of their own.
  private lazy val seedRound: Int =
    await(db.run(sql"select id from contest_rounds where testing_system_id = 62042".as[Int].head))
  private lazy val season: Int = await(db.run(sql"select season_id from contest_rounds where testing_system_id = 62042".as[Int].head))

  private var round: Int = 0
  /** A participant with an active credential in the fixture round, and one without any credential. */
  private var kid: Int = 0
  private var stranger: Int = 0

  private def participantOf(login: String): Int = login match {
    case "team1_1" | "team1_3" => kid
    case other => fail(s"unexpected $other")
  }

  private def team(participant: Int): LoggedInTeam = {
    val c = await(Rounds.currentRound(db, round)).get
    LoggedInTeam("x", c, Team(participant, 1, "P", "S", "C", None), Seq.empty)
  }

  private val t0 = new DateTime(2030, 1, 1, 12, 0, 0, 0)
  private val source = "int main() {}".getBytes("UTF-8")

  private def lang(module: String = "g++"): Int =
    await(db.run(sql"select id from season_languages where season_id = $season and module_id = $module".as[Int].head))

  private def cleanup(): Unit = {
    exec(sqlu"delete from submits where round_id in (select id from contest_rounds where name = 'Submit spec round')")
    exec(sqlu"delete from problems where round_id in (select id from contest_rounds where name = 'Submit spec round')")
    exec(sqlu"delete from round_credentials where round_id in (select id from contest_rounds where name = 'Submit spec round')")
    exec(sqlu"delete from participants where full_name in ('Submit Spec Kid', 'Submit Spec Stranger')")
    exec(sqlu"delete from contest_rounds where name = 'Submit spec round'")
  }

  /** Creates the fixture round with the given times and problem `Z`, runs the body, then deletes everything. */
  private def withRound(starts: Option[DateTime], ends: Option[DateTime], finished: Boolean = false)(body: => Unit): Unit = {
    assume(url.isDefined)
    val season = this.season // not inside an interpolation: the lazy val awaits the database
    val seedRound = this.seedRound
    cleanup()
    val s = starts.map(d => new java.sql.Timestamp(d.getMillis)).orNull
    val e = ends.map(d => new java.sql.Timestamp(d.getMillis)).orNull
    try {
      exec(sqlu"""insert into contest_rounds (contest_id, season_id, round_seq, season_seq, name, starts_at, ends_at, is_finished)
                  select c.id, c.season_id, 80, 80, 'Submit spec round', $s, $e, $finished
                    from contests c where c.season_id = $season""")
      round = await(db.run(sql"select id from contest_rounds where name = 'Submit spec round'".as[Int].head))
      for ((name, seq) <- Seq("Submit Spec Kid" -> 98, "Submit Spec Stranger" -> 97))
        exec(sqlu"""insert into participants (season_id, teacher_id, season_seq, full_name, school_id, grade, contest_id)
                    select p.season_id, p.teacher_id, $seq, $name, p.school_id, 9, p.contest_id
                      from participants p where p.id = (select participant_id from round_credentials where round_id = $seedRound and login = 'team1_1')""")
      kid = await(db.run(sql"select id from participants where full_name = 'Submit Spec Kid'".as[Int].head))
      stranger = await(db.run(sql"select id from participants where full_name = 'Submit Spec Stranger'".as[Int].head))
      exec(sqlu"""insert into round_credentials (round_id, contest_id, participant_id, login, password)
                  select r.id, r.contest_id, $kid, 'submit_kid', '777000' from contest_rounds r where r.id = $round""")
      exec(sqlu"insert into problems (season_id, round_id, problem_id, name, tests, max_points) values ($season, $round, 'Z', 'Zed', 3, 10)")
      body
    } finally cleanup()
  }

  private def running(body: => Unit): Unit = withRound(Some(t0), Some(t0.plusHours(2)))(body)
  private val inside = t0.plusHours(1)

  private def insert(p: Int, problem: String = "Z", language: Int = lang(), now: DateTime = inside) =
    await(Submits.insert(db, team(p), problem, language, source, Some("127.0.0.1"), now))

  private def count: Int = await(db.run(sql"select count(*) from submits where round_id = $round".as[Int].head))

  test("accepted while the round runs: the row has the expected fields and one submit_new arrives") {
    running {
      val conn = DriverManager.getConnection(url.get, sys.env.getOrElse("QF_TEST_USER", ""), sys.env.getOrElse("QF_TEST_PASSWORD", ""))
      try {
        conn.createStatement().execute("LISTEN submit_new")
        val p = participantOf("team1_1")
        val id = insert(p).getOrElse(fail("rejected"))
        val ns = conn.unwrap(classOf[org.postgresql.PGConnection]).getNotifications(2000)
        assert(ns != null && ns.length == 1 && ns(0).getParameter == id.toString)
        val row = await(db.run(
          sql"""select season_id, round_id, participant_id, problem_id, language_id, submitted_at = ${new java.sql.Timestamp(inside.getMillis)},
                       host(remote_addr), tested, compiled, passed, taken, points is null, testing_id is null, length(source)
                  from submits where id = $id"""
            .as[(Int, Int, Int, String, Int, Boolean, String, Boolean, Boolean, Int, Int, Boolean, Boolean, Int)].head))
        assert(row == ((season, round, p, "Z", lang(), true, "127.0.0.1", false, false, 0, 0, true, true, source.length)))
      } finally conn.close()
    }
  }

  test("the end is exclusive: ends_at - 1ms is accepted, ends_at is not") {
    running {
      val p = participantOf("team1_1")
      assert(insert(p, now = t0.plusHours(2).minusMillis(1)).isRight)
      assert(insert(p, now = t0.plusHours(2)) == Left(RoundNotRunning))
      assert(insert(p, now = t0.plusHours(2).plusMillis(1)) == Left(RoundNotRunning))
      assert(count == 1)
    }
  }

  test("the start is inclusive; before it the round is not running") {
    running {
      val p = participantOf("team1_1")
      assert(insert(p, now = t0.minusMillis(1)) == Left(RoundNotRunning))
      assert(insert(p, now = t0).isRight)
    }
  }

  test("is_finished rejects even before ends_at, read from the database not the session") {
    running {
      val p = participantOf("team1_1")
      val stale = team(p) // loaded while the round was not finished
      exec(sqlu"update contest_rounds set is_finished = true where id = $round")
      assert(await(Submits.insert(db, stale, "Z", lang(), source, None, inside)) == Left(RoundNotRunning))
      assert(count == 0)
    }
  }

  test("currentRounds fills isFinished and acceptsSubmits follows the same boundaries") {
    running {
      val c = await(Rounds.currentRound(db, round)).get
      assert(!c.isFinished && c.acceptsSubmits(t0) && !c.acceptsSubmits(t0.minusMillis(1)))
      assert(c.acceptsSubmits(t0.plusHours(2).minusMillis(1)) && !c.acceptsSubmits(t0.plusHours(2)))
      exec(sqlu"update contest_rounds set is_finished = true where id = $round")
      val f = await(Rounds.currentRound(db, round)).get
      assert(f.isFinished && !f.acceptsSubmits(inside))
      assert(await(Rounds.currentRounds(db)).find(_.id == round).exists(_.isFinished))
    }
  }

  test("a round with no times is not running, without an exception") {
    withRound(None, None) {
      val p = participantOf("team1_1")
      val c = Contest(round, "r", t0, t0.plusHours(2), None, season)
      assert(await(Submits.insert(db, LoggedInTeam("x", c, Team(p, 1, "P", "S", "C", None), Seq.empty), "Z", lang(), source, None, inside)) == Left(RoundNotRunning))
    }
    withRound(Some(t0), None) {
      val c = Contest(round, "r", t0, t0.plusHours(2), None, season)
      assert(await(Submits.insert(db, LoggedInTeam("x", c, Team(participantOf("team1_1"), 1, "P", "S", "C", None), Seq.empty), "Z", lang(), source, None, inside)) == Left(RoundNotRunning))
    }
  }

  test("an unknown problem is UnknownProblem") {
    running {
      assert(insert(participantOf("team1_1"), problem = "nope") == Left(UnknownProblem))
      assert(count == 0)
    }
  }

  test("a language of another season is UnknownLanguage; so is an unknown id") {
    running {
      try {
        exec(sqlu"""insert into seasons (short_id, name, is_current, reg_deadline, reg_code) values ('qf-sub-old', 'Old', false, now(), 'x')""")
        exec(sqlu"""insert into season_languages (season_id, name, module_id) select id, 'foreign', 'f' from seasons where short_id = 'qf-sub-old'""")
        val foreign = await(db.run(sql"select id from season_languages where name = 'foreign'".as[Int].head))
        assert(insert(participantOf("team1_1"), language = foreign) == Left(UnknownLanguage))
        assert(insert(participantOf("team1_1"), language = -5) == Left(UnknownLanguage))
        assert(count == 0)
      } finally {
        exec(sqlu"delete from season_languages where name = 'foreign'")
        exec(sqlu"delete from seasons where short_id = 'qf-sub-old'")
      }
    }
  }

  test("a participant without a credential for the round, or with an inactive one, is NotAdmitted") {
    running {
      assert(insert(stranger) == Left(NotAdmitted))
      try {
        exec(sqlu"update round_credentials set is_active = false where participant_id = $kid and round_id = $round")
        assert(insert(kid) == Left(NotAdmitted))
        exec(sqlu"update round_credentials set is_active = true where participant_id = $kid and round_id = $round")
        assert(insert(kid).isRight)
      } finally exec(sqlu"update round_credentials set is_active = true where participant_id = $kid and round_id = $round")
      assert(count == 1)
    }
  }

  test("season_id comes from the round, not from the session's Contest") {
    running {
      val wrong = Contest(round, "r", t0, t0.plusHours(2), None, season + 1000)
      val id = await(Submits.insert(db, LoggedInTeam("x", wrong, Team(kid, 1, "P", "S", "C", None), Seq.empty), "Z", lang(), source, None, inside))
        .getOrElse(fail("rejected"))
      assert(await(db.run(sql"select season_id from submits where id = $id".as[Int].head)) == season)
    }
  }

  test("a constraint violation that slips past the checks maps to the error of the constraint") {
    running {
      val p = participantOf("team1_1")
      val e = SubmitConstraints
      assert(e.errorOf("fk_submits_credential") == Some(NotAdmitted))
      assert(e.errorOf("fk_submits_problem") == Some(UnknownProblem))
      assert(e.errorOf("fk_submits_language_season") == Some(UnknownLanguage))
      assert(e.errorOf("fk_submits_round_season") == None)
      // the real thing: a raw insert with a bad problem id raises 23503 with the constraint name
      val ex = intercept[Exception](exec(sqlu"""insert into submits (season_id, round_id, participant_id, problem_id, language_id, source)
                                               values ($season, $round, $p, 'nope', ${lang()}, decode('00', 'hex'))"""))
      assert(SubmitConstraints.fromException(ex) == Some(UnknownProblem))
    }
  }
}
