package models

import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.api.util.Credentials
import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Team and admin login against the teacher-web schema. Skipped unless QF_TEST_URL is set; expects the database
  * migrated by `teacherweb migrate` and seeded with teacher-web's doc/testdata/seed.sql (admin admin@example.org
  * with the bcrypt hash of 'secret', credentials team1_1..3):
  *   QF_TEST_URL=jdbc:postgresql://127.0.0.1:55433/contester QF_TEST_USER=postgres QF_TEST_PASSWORD=x
  * Every test puts back what it changed.
  */
class AuthDbSpec extends AnyFunSuite {
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

  private def passwordOf(login: String): String =
    await(db.run(sql"select password from round_credentials where login = $login".as[String].head))

  private def seedRound: Int =
    await(db.run(sql"select id from contest_rounds where testing_system_id = 62042".as[Int].head))

  private def participantOf(login: String): Int =
    await(db.run(sql"select participant_id from round_credentials where login = $login".as[Int].head))

  private def login(l: String, p: String) = await(Users.authenticate(db, l, p))

  test("a team logs in with its login and plaintext password; the identity carries participant and round") {
    assume(url.isDefined)
    val pw = passwordOf("team1_2")
    val t = login("team1_2", pw).getOrElse(fail("valid login rejected"))
    assert(t.username == "team1_2")
    assert(t.team.id == participantOf("team1_2"))
    assert(t.contest.id == seedRound)
    assert(await(Users.resolve(db, "team1_2")).exists(_.team.id == t.team.id))
  }

  test("the login is case-insensitive, the password is not") {
    assume(url.isDefined)
    val pw = passwordOf("team1_1")
    assert(login("TEAM1_1", pw).exists(_.team.id == participantOf("team1_1")))
    assert(login(" Team1_1 ", pw).isDefined)
    assert(await(Users.resolve(db, "TEAM1_1")).isDefined)
    assert(login("team1_1", pw.reverse + "x").isEmpty)
    assert(login("team1_1", "").isEmpty)
    assert(login("nobody", pw).isEmpty)
  }

  test("an inactive credential does not log in") {
    assume(url.isDefined)
    val pw = passwordOf("team1_3")
    try {
      exec(sqlu"update round_credentials set is_active = false where login = 'team1_3'")
      assert(login("team1_3", pw).isEmpty)
      assert(await(Users.resolve(db, "team1_3")).isEmpty)
    } finally exec(sqlu"update round_credentials set is_active = true where login = 'team1_3'")
    assert(login("team1_3", pw).isDefined)
  }

  test("a soft-deleted participant does not log in") {
    assume(url.isDefined)
    val pw = passwordOf("team1_3")
    val p = participantOf("team1_3")
    try {
      exec(sqlu"update participants set is_deleted = true where id = $p")
      assert(login("team1_3", pw).isEmpty)
    } finally exec(sqlu"update participants set is_deleted = false where id = $p")
    assert(login("team1_3", pw).isDefined)
  }

  test("a round that has ended still accepts logins") {
    assume(url.isDefined)
    val r = seedRound
    val pw = passwordOf("team1_1")
    try {
      exec(sqlu"update contest_rounds set is_finished = true where id = $r")
      assert(login("team1_1", pw).isDefined)
    } finally exec(sqlu"update contest_rounds set is_finished = false where id = $r")
  }

  test("a credential of a previous season never logs in") {
    assume(url.isDefined)
    val short = "old-auth-spec"
    def cleanup() = exec(sqlu"delete from seasons where short_id = $short")
    cleanup()
    try {
      exec(sqlu"""insert into seasons (short_id, name, is_current, reg_deadline, reg_code)
                  values ($short, 'Old', false, now(), 'x')""")
      exec(sqlu"""insert into contests (season_id, season_seq, name)
                  select id, 1, 'Old contest' from seasons where short_id = $short""")
      exec(sqlu"""insert into contest_rounds (contest_id, season_id, round_seq, season_seq, name, starts_at, ends_at)
                  select c.id, c.season_id, 1, 1, 'Old round', now() - interval '1 hour', now() + interval '1 hour'
                  from contests c join seasons s on s.id = c.season_id where s.short_id = $short""")
      exec(sqlu"""insert into teachers (season_id, season_seq, email, password_hash, full_name, phone, school_id)
                  select s.id, 1, 'old@example.org', 'x', 'Old Teacher', '1', (select min(id) from schools)
                  from seasons s where s.short_id = $short""")
      exec(sqlu"""insert into participants (season_id, teacher_id, season_seq, full_name, school_id, grade, contest_id)
                  select s.id, t.id, 1, 'Old Kid', t.school_id, 9, c.id
                  from seasons s join teachers t on t.season_id = s.id join contests c on c.season_id = s.id
                  where s.short_id = $short""")
      exec(sqlu"""insert into round_credentials (round_id, contest_id, participant_id, login, password)
                  select r.id, r.contest_id, p.id, 'old1_1', '111111'
                  from contest_rounds r join participants p on p.contest_id = r.contest_id
                  join seasons s on s.id = r.season_id where s.short_id = $short""")
      assert(await(db.run(sql"select count(*) from round_credentials where login = 'old1_1'".as[Int].head)) == 1)
      assert(login("old1_1", "111111").isEmpty)
      assert(await(Users.resolve(db, "old1_1")).isEmpty)
    } finally cleanup()
  }

  // ---- admins ----

  private val seedAdmin = "admin@example.org"

  test("the seed admin's Go bcrypt hash (2a, cost 12) verifies; a wrong password does not") {
    assume(url.isDefined)
    val a = await(Admins.authenticate(db, seedAdmin, "secret")).getOrElse(fail("valid admin rejected"))
    assert(a.email == seedAdmin)
    assert(a.passwordHash.startsWith("$2a$12$"))
    assert(await(Admins.authenticate(db, seedAdmin, "Secret")).isEmpty)
    assert(await(Admins.authenticate(db, seedAdmin, "")).isEmpty)
    assert(await(Admins.authenticate(db, "nobody@example.org", "secret")).isEmpty)
  }

  test("the admin email is matched case-insensitively and trimmed, like teacher-web's LOWER(BTRIM(email))") {
    assume(url.isDefined)
    assert(await(Admins.authenticate(db, "ADMIN@Example.ORG", "secret")).exists(_.email == seedAdmin))
    assert(await(Admins.authenticate(db, "  admin@example.org ", "secret")).isDefined)
  }

  test("the provider and the identity service agree, and a changed hash ends the session") {
    assume(url.isDefined)
    val provider = new AdminsProvider(dbConfig)
    val svc = new AdminsServiceImpl(dbConfig)
    assert(await(provider.authenticate(Credentials(seedAdmin, "wrong"))).isEmpty)
    val li = await(provider.authenticate(Credentials("ADMIN@example.org", "secret"))).get
    assert(li.providerKey.startsWith(seedAdmin + ":"))
    assert(await(svc.retrieve(li)).exists(_.email == seedAdmin))
    assert(await(svc.retrieve(LoginInfo("admin", seedAdmin + ":deadbeef"))).isEmpty)
    val old = await(db.run(sql"select password_hash from admins where email = $seedAdmin".as[String].head))
    try {
      exec(sqlu"update admins set password_hash = ${controllers.PasswordHash.hash("other")} where email = $seedAdmin")
      assert(await(svc.retrieve(li)).isEmpty)
    } finally exec(sqlu"update admins set password_hash = $old where email = $seedAdmin")
    assert(await(svc.retrieve(li)).isDefined)
  }
}
