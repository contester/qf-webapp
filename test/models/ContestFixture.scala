package models

import com.typesafe.config.ConfigFactory
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * A round of its own (2030, current season), one participant with an active credential in it and problem `Z`,
  * for DB specs that must not touch the seed round. Needs QF_TEST_URL (see SubmitInsertDbSpec); `cleanup` deletes
  * everything named after `tag`, including submits, testings and custom tests of the round.
  */
class ContestFixture(tag: String)(implicit ec: ExecutionContext) {
  val url: Option[String] = sys.env.get("QF_TEST_URL")
  val user: String = sys.env.getOrElse("QF_TEST_USER", "")
  val password: String = sys.env.getOrElse("QF_TEST_PASSWORD", "")

  lazy val dbConfig: DatabaseConfig[JdbcProfile] = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "$user", password = "$password", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))

  lazy val db = dbConfig.db

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  def await[T](f: Future[T]): T = Await.result(f, 60.seconds)
  def exec(a: DBIO[Int]): Int = await(db.run(a))
  def one[T](a: DBIO[Seq[T]]): T = await(db.run(a)).head

  val roundName = s"$tag round"
  val nullRoundName = s"$tag null round"
  val kidName = s"$tag Kid"

  var season = 0
  var round = 0
  var nullRound = 0
  var kid = 0
  var language = 0

  def cleanup(): Unit = {
    val rn = roundName
    val nn = nullRoundName
    val kn = kidName
    val rounds = sql"select id from contest_rounds where name in ($rn, $nn)".as[Int]
    for (r <- await(db.run(rounds))) {
      exec(sqlu"delete from results where testing_id in (select t.id from testings t join submits s on s.id = t.submit_id where s.round_id = $r)")
      exec(sqlu"delete from testing_groups where testing_id in (select t.id from testings t join submits s on s.id = t.submit_id where s.round_id = $r)")
      exec(sqlu"update submits set testing_id = null where round_id = $r")
      exec(sqlu"delete from testings where submit_id in (select id from submits where round_id = $r)")
      exec(sqlu"delete from submits where round_id = $r")
      exec(sqlu"delete from custom_tests where round_id = $r")
      exec(sqlu"delete from problems where round_id = $r")
      exec(sqlu"delete from round_credentials where round_id = $r")
      exec(sqlu"delete from contest_rounds where id = $r")
    }
    exec(sqlu"delete from participants where full_name = $kn")
  }

  /** Creates the round, the null-times round, the participant, its credential and problem Z. */
  def create(): Unit = {
    cleanup()
    val seedRound = one(sql"select id from contest_rounds where testing_system_id = 62042".as[Int])
    season = one(sql"select season_id from contest_rounds where id = $seedRound".as[Int])
    val s = season
    val rn = roundName
    val nn = nullRoundName
    val kn = kidName
    exec(sqlu"""insert into contest_rounds (contest_id, season_id, round_seq, season_seq, name, starts_at, ends_at)
                select c.id, c.season_id, 81, 81, $rn, timestamptz '2030-01-01 12:00:00+00', timestamptz '2030-01-01 14:00:00+00'
                  from contests c where c.season_id = $s limit 1""")
    exec(sqlu"""insert into contest_rounds (contest_id, season_id, round_seq, season_seq, name)
                select c.id, c.season_id, 82, 82, $nn from contests c where c.season_id = $s limit 1""")
    round = one(sql"select id from contest_rounds where name = $rn".as[Int])
    nullRound = one(sql"select id from contest_rounds where name = $nn".as[Int])
    val r = round
    exec(sqlu"""insert into participants (season_id, teacher_id, season_seq, full_name, school_id, grade, contest_id)
                select p.season_id, p.teacher_id, 96, $kn, p.school_id, 9, p.contest_id
                  from participants p where p.id = (select participant_id from round_credentials where round_id = $seedRound and login = 'team1_1')""")
    kid = one(sql"select id from participants where full_name = $kn".as[Int])
    val k = kid
    val login = s"${tag.toLowerCase.replaceAll("[^a-z]", "")}_kid"
    exec(sqlu"""insert into round_credentials (round_id, contest_id, participant_id, login, password)
                select r.id, r.contest_id, $k, $login, '777000' from contest_rounds r where r.id = $r""")
    exec(sqlu"insert into problems (season_id, round_id, problem_id, name, tests, max_points) values ($s, $r, 'Z', 'Zed', 3, 10)")
    language = one(sql"select id from season_languages where season_id = $s order by id limit 1".as[Int])
  }

  def withFixture[T](body: => T): T = {
    create()
    try body finally cleanup()
  }

  def insertSubmit(): Long = {
    val (s, r, k, l) = (season, round, kid, language)
    one(sql"""insert into submits (season_id, round_id, participant_id, problem_id, language_id, source)
              values ($s, $r, $k, 'Z', $l, decode('00', 'hex')) returning id""".as[Long])
  }

  def insertTesting(submit: Long): Long =
    one(sql"insert into testings (submit_id) values ($submit) returning id".as[Long])

  def insertCustomTest(): Long = {
    val (s, r, k, l) = (season, round, kid, language)
    one(sql"""insert into custom_tests (season_id, round_id, participant_id, language_id, source, input)
              values ($s, $r, $k, $l, 'src'::bytea, 'in'::bytea) returning id""".as[Long])
  }
}
