package models

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.NotRated
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The points board as the pages load it (MonitorPoints.load). Skipped unless QF_TEST_URL is set (see
  * SubmitInsertDbSpec). Uses a round, participants and problems of its own and deletes them afterwards.
  */
class MonitorPointsDbSpec extends AnyFunSuite {
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

  private val kids = Seq("Board Spec One" -> 91, "Board Spec Two" -> 92, "Board Spec Unrated" -> 93, "Board Spec Dsq" -> 94)

  private def cleanup(): Unit = {
    exec(sqlu"delete from submits where round_id in (select id from contest_rounds where name = 'Board spec round')")
    exec(sqlu"delete from problems where round_id in (select id from contest_rounds where name = 'Board spec round')")
    exec(sqlu"delete from round_credentials where round_id in (select id from contest_rounds where name = 'Board spec round')")
    exec(sqlu"delete from participants where full_name like 'Board Spec %'")
    exec(sqlu"delete from contest_rounds where name = 'Board spec round'")
  }

  test("the loaded board: best finished points per problem, full at max_points, ranked, unrated last, disqualified hidden") {
    assume(url.isDefined)
    val seedRound = await(db.run(sql"select id from contest_rounds where testing_system_id = 62042".as[Int].head))
    val season = await(db.run(sql"select season_id from contest_rounds where id = $seedRound".as[Int].head))
    val language = await(db.run(sql"select id from season_languages where season_id = $season and module_id = 'g++'".as[Int].head))
    cleanup()
    try {
      exec(sqlu"""insert into contest_rounds (contest_id, season_id, round_seq, season_seq, name, starts_at, ends_at, is_finished)
                  select c.id, c.season_id, 82, 82, 'Board spec round', now() - interval '3 hours', now() - interval '1 hour', false
                    from contests c where c.season_id = $season""")
      val round = await(db.run(sql"select id from contest_rounds where name = 'Board spec round'".as[Int].head))
      val ids = kids.map { case (name, seq) =>
        exec(sqlu"""insert into participants (season_id, teacher_id, season_seq, full_name, school_id, grade, contest_id)
                    select p.season_id, p.teacher_id, $seq, $name, p.school_id, 9, p.contest_id
                      from participants p where p.id = (select participant_id from round_credentials where round_id = $seedRound and login = 'team1_1')""")
        val id = await(db.run(sql"select id from participants where full_name = $name".as[Int].head))
        val login = s"board_$seq"
        exec(sqlu"""insert into round_credentials (round_id, contest_id, participant_id, login, password)
                    select r.id, r.contest_id, $id, $login, '777002' from contest_rounds r where r.id = $round""")
        id
      }
      val Seq(one, two, unrated, dsq) = ids
      exec(sqlu"update round_credentials set unrated = 'not_rated' where round_id = $round and participant_id = $unrated")
      exec(sqlu"update round_credentials set unrated = 'disqualified' where round_id = $round and participant_id = $dsq")
      exec(sqlu"""insert into problems (season_id, round_id, problem_id, name, tests, max_points) values
                  ($season, $round, 'P', 'Pe', 5, 20), ($season, $round, 'Q', 'Qu', 5, 25)""")

      def submit(p: Int, problem: String, tested: Boolean, compiled: Boolean, points: Option[Double]): Unit = {
        exec(sqlu"""insert into submits (season_id, round_id, participant_id, problem_id, language_id, source, tested, compiled, points)
                    values ($season, $round, $p, $problem, $language, 'x'::bytea, $tested, $compiled, $points)""")
      }
      // one: P 5 then 20 (full), Q 10 and one still testing => 30
      submit(one, "P", tested = true, compiled = true, Some(5))
      submit(one, "P", tested = true, compiled = true, Some(20))
      submit(one, "Q", tested = true, compiled = true, Some(10))
      submit(one, "Q", tested = false, compiled = false, None)
      // two: Q 25 (full); P a compile error and a finished one whose points are not written yet => 25
      submit(two, "Q", tested = true, compiled = true, Some(25))
      submit(two, "P", tested = true, compiled = false, None)
      submit(two, "P", tested = true, compiled = true, None)
      // unrated: 40, more than anyone, listed last without a place; disqualified: hidden
      submit(unrated, "P", tested = true, compiled = true, Some(20))
      submit(unrated, "Q", tested = true, compiled = true, Some(20))
      submit(dsq, "P", tested = true, compiled = true, Some(20))

      val board = await(MonitorPoints.load(db, round))
      assert(board.problems == Seq(ProblemColumn("P", "Pe", 20), ProblemColumn("Q", "Qu", 25)))

      def team(id: Int, number: Int, name: String, u: Option[org.stingray.contester.dbmodel.Unrated] = None) = {
        val t = board.rows.find(_.team.participantId == id).get.team
        assert(t.number == number && t.name == name && t.unrated == u)
        t
      }
      assert(board.rows == Seq(
        RankedRow(Some(1), team(one, 91, "Board Spec One"), Seq(PointsCell(Some(20), 2, full = true), PointsCell(Some(10), 2, full = false)), 30),
        RankedRow(Some(2), team(two, 92, "Board Spec Two"), Seq(PointsCell(None, 2, full = false), PointsCell(Some(25), 1, full = true)), 25),
        RankedRow(None, team(unrated, 93, "Board Spec Unrated", Some(NotRated)), Seq(PointsCell(Some(20), 1, full = true), PointsCell(Some(20), 1, full = false)), 40)))
      assert(!board.rows.exists(_.team.participantId == dsq))

      // a rejudge writes new points into the same row: the board follows, it never adds old and new
      exec(sqlu"update submits set points = 3 where round_id = $round and participant_id = $one and problem_id = 'P' and points = 20")
      val after = await(MonitorPoints.load(db, round))
      assert(after.rows.map(r => (r.place, r.team.participantId, r.total)) ==
        Seq((Some(1), two, 25.0), (Some(2), one, 15.0), (None, unrated, 40.0)))
    } finally cleanup()
  }

}
