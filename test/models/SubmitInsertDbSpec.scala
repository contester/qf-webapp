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

  private lazy val round: Int =
    await(db.run(sql"select id from contest_rounds where testing_system_id = 62042".as[Int].head))
  private lazy val season: Int = await(db.run(sql"select season_id from contest_rounds where id = $round".as[Int].head))

  private def participantOf(login: String): Int =
    await(db.run(sql"select participant_id from round_credentials where login = $login".as[Int].head))

  private def team(participant: Int): LoggedInTeam = {
    val c = await(Rounds.currentRound(db, round)).get
    LoggedInTeam("x", c, Team(participant, 1, "P", "S", "C", None), Seq.empty)
  }

  private val t0 = new DateTime(2030, 1, 1, 12, 0, 0, 0)
  private val source = "int main() {}".getBytes("UTF-8")

  private def lang(module: String = "g++"): Int =
    await(db.run(sql"select id from season_languages where season_id = $season and module_id = $module".as[Int].head))

  /** Sets the round times and a problem `Z`, runs the body, then restores everything. */
  private def withRound(starts: Option[DateTime], ends: Option[DateTime], finished: Boolean = false)(body: => Unit): Unit = {
    assume(url.isDefined)
    val (os, oe, of) = await(db.run(
      sql"select starts_at, ends_at, is_finished from contest_rounds where id = $round".as[(Option[java.sql.Timestamp], Option[java.sql.Timestamp], Boolean)].head))
    val s = starts.map(d => new java.sql.Timestamp(d.getMillis)).orNull
    val e = ends.map(d => new java.sql.Timestamp(d.getMillis)).orNull
    try {
      exec(sqlu"update contest_rounds set starts_at = $s, ends_at = $e, is_finished = $finished where id = $round")
      exec(sqlu"insert into problems (season_id, round_id, problem_id, name, tests, max_points) values ($season, $round, 'Z', 'Zed', 3, 10)")
      body
    } finally {
      exec(sqlu"delete from submits where round_id = $round")
      exec(sqlu"delete from problems where round_id = $round")
      exec(sqlu"update contest_rounds set starts_at = $os, ends_at = $oe, is_finished = $of where id = $round")
    }
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
      val p = participantOf("team1_3")
      try {
        exec(sqlu"update round_credentials set is_active = false where participant_id = $p and round_id = $round")
        assert(insert(p) == Left(NotAdmitted))
        exec(sqlu"update round_credentials set is_active = true where participant_id = $p and round_id = $round")
        assert(insert(p).isRight)
        exec(sqlu"""insert into participants (season_id, contest_id, season_seq, full_name, school_id, teacher_id, grade)
                    select season_id, contest_id, 99, 'Nobody', school_id, teacher_id, grade from participants where id = $p""")
        val nobody = await(db.run(sql"select id from participants where full_name = 'Nobody'".as[Int].head))
        assert(insert(nobody) == Left(NotAdmitted))
      } finally {
        exec(sqlu"update round_credentials set is_active = true where participant_id = $p and round_id = $round")
        exec(sqlu"delete from participants where full_name = 'Nobody'")
      }
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
