package models

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Disqualified, NotRated, Team, Unrated}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Rounds and teams read from the teacher-web schema. Skipped unless QF_TEST_URL is set; expects the database
  * migrated by `teacherweb migrate` and seeded with teacher-web's doc/testdata/seed.sql:
  *   QF_TEST_URL=jdbc:postgresql://127.0.0.1:55433/contester QF_TEST_USER=postgres QF_TEST_PASSWORD=x
  * Every test puts back what it changed.
  */
class ContestRoundsDbSpec extends AnyFunSuite {
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

  /** The seeded round: Polygon contest 62042 in the current season. */
  private def seedRound: Int =
    await(db.run(sql"select id from contest_rounds where testing_system_id = 62042".as[Int].head))

  private def participantOf(login: String): Int =
    await(db.run(sql"select participant_id from round_credentials where login = $login".as[Int].head))

  test("currentRounds lists the seeded round with its contest, times and polygon id") {
    assume(url.isDefined)
    val rounds = await(Rounds.currentRounds(db))
    val r = rounds.find(_.id == seedRound).getOrElse(fail(s"seed round missing from $rounds"))
    assert(r.name == "Test Contest — Round 1")
    assert(r.polygonId.contains("62042"))
    assert(r.startTime.isBeforeNow && r.endTime.isAfterNow)
    val season = await(db.run(sql"select id from seasons where is_current".as[Int].head))
    assert(r.seasonId == season)
  }

  test("currentRounds leaves out rounds of other seasons and rounds without an end") {
    assume(url.isDefined)
    val season = await(db.run(sql"select id from seasons where is_current".as[Int].head))
    val contest = await(db.run(sql"select id from contests where season_id = $season".as[Int].head))
    try {
      exec(sqlu"""insert into seasons (short_id, name, is_current, reg_deadline, reg_code)
                  values ('qf-old', 'Old season', false, now(), 'x')""")
      exec(sqlu"""insert into contests (season_id, season_seq, name)
                  select id, 1, 'Old contest' from seasons where short_id = 'qf-old'""")
      exec(sqlu"""insert into contest_rounds (contest_id, season_id, round_seq, season_seq, name, starts_at, ends_at)
                  select c.id, c.season_id, 1, 1, 'Old round', now() - interval '1 hour', now() + interval '1 hour'
                    from contests c join seasons s on s.id = c.season_id where s.short_id = 'qf-old'""")
      exec(sqlu"""insert into contest_rounds (contest_id, season_id, round_seq, season_seq, name, starts_at, ends_at)
                  values ($contest, $season, 90, 90, 'No end', now() - interval '1 hour', null)""")
      exec(sqlu"""insert into contest_rounds (contest_id, season_id, round_seq, season_seq, name, starts_at, ends_at)
                  values ($contest, $season, 91, 91, 'No start', null, now() + interval '1 hour')""")
      val names = await(Rounds.currentRounds(db)).map(_.name)
      assert(names.contains("Test Contest — Round 1"))
      assert(!names.exists(_.contains("Old")), names)
      assert(!names.exists(_.contains("No end")), names)
      assert(!names.exists(_.contains("No start")), names)
    } finally {
      exec(sqlu"delete from contest_rounds where contest_id = $contest and round_seq in (90, 91)")
      exec(sqlu"""delete from contest_rounds where season_id in (select id from seasons where short_id = 'qf-old')""")
      exec(sqlu"""delete from contests where season_id in (select id from seasons where short_id = 'qf-old')""")
      exec(sqlu"delete from seasons where short_id = 'qf-old'")
    }
  }

  test("teamsOfRound reads number, name, school and city") {
    assume(url.isDefined)
    val teams = await(Rounds.teamsOfRound(db, seedRound)).sortBy(_.number)
    assert(teams.map(_.number) == Seq(1, 2, 3))
    val first = teams.head
    assert(first == Team(participantOf("team1_1"), 1, "Participant 1", "Test School", "Test City", None))
  }

  test("teamsOfRound leaves out soft-deleted participants and inactive credentials") {
    assume(url.isDefined)
    val deleted = participantOf("team1_2")
    val inactive = participantOf("team1_3")
    try {
      exec(sqlu"update participants set is_deleted = true where id = $deleted")
      exec(sqlu"update round_credentials set is_active = false where participant_id = $inactive")
      val ids = await(Rounds.teamsOfRound(db, seedRound)).map(_.participantId)
      assert(ids == Seq(participantOf("team1_1")))
    } finally {
      exec(sqlu"update participants set is_deleted = false where id = $deleted")
      exec(sqlu"update round_credentials set is_active = true where participant_id = $inactive")
    }
  }

  test("unrated maps NULL, not_rated and disqualified both ways") {
    assume(url.isDefined)
    import org.stingray.contester.dbmodel.SlickModel
    val round = seedRound
    val p = participantOf("team1_1")
    def readSlick: Option[Unrated] = await(Rounds.teamsOfRound(db, round)).find(_.participantId == p).get.unrated
    def readRaw: Option[String] =
      await(db.run(sql"select unrated::text from round_credentials where participant_id = $p and round_id = $round".as[Option[String]].head))
    def writeSlick(v: Option[Unrated]): Int =
      await(db.run(SlickModel.roundCredentials.filter(x => x.participantId === p && x.roundId === round).map(_.unrated).update(v)))
    try {
      assert(readSlick.isEmpty && readRaw.isEmpty)
      exec(sqlu"update round_credentials set unrated = 'not_rated' where participant_id = $p and round_id = $round")
      assert(readSlick.contains(NotRated))
      exec(sqlu"update round_credentials set unrated = 'disqualified' where participant_id = $p and round_id = $round")
      assert(readSlick.contains(Disqualified))

      assert(writeSlick(Some(NotRated)) == 1)
      assert(readRaw.contains("not_rated"))
      assert(writeSlick(Some(Disqualified)) == 1)
      assert(readRaw.contains("disqualified"))
      assert(writeSlick(None) == 1)
      assert(readRaw.isEmpty)
      // filtering by the enum works too
      exec(sqlu"update round_credentials set unrated = 'not_rated' where participant_id = $p and round_id = $round")
      val notRated = await(db.run(SlickModel.roundCredentials
        .filter(x => x.roundId === round && x.unrated === (NotRated: Unrated)).map(_.participantId).result))
      assert(notRated == Seq(p))
    } finally {
      exec(sqlu"update round_credentials set unrated = null where participant_id = $p and round_id = $round")
    }
  }
}

class RoundNameSpec extends AnyFunSuite {
  test("contest and round names are joined") {
    assert(Rounds.roundName("Test Contest", "Round 1") == "Test Contest — Round 1")
  }

  test("an empty round name or one equal to the contest's leaves the contest name") {
    assert(Rounds.roundName("Olympiad", "  ") == "Olympiad")
    assert(Rounds.roundName("Olympiad", "Olympiad") == "Olympiad")
  }
}

class TeamNamesSpec extends AnyFunSuite {
  private val team = Team(7, 12, "Ivan Petrov", "School 1", "Tomsk", None)

  test("the full name has the number, the name and the school with its city") {
    assert(team.teamFullName == "12. Ivan Petrov (School 1, Tomsk)")
  }

  test("disqualified is not rated and on no board; not rated stays on the board") {
    assert(!team.notRated && !team.disqualified && !team.disabled)
    val nr = team.copy(unrated = Some(NotRated))
    assert(nr.notRated && !nr.disqualified && !nr.disabled)
    val dq = team.copy(unrated = Some(Disqualified))
    assert(dq.notRated && dq.disqualified && dq.disabled)
  }
}
