package models

import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.api.util.Credentials
import com.typesafe.config.ConfigFactory
import controllers.PasswordHash
import org.scalatest.funsuite.AnyFunSuite
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

/**
  * Admin login against a real database with an admins table (skipped unless ADMINS_TEST_URL is set):
  *   ADMINS_TEST_URL=jdbc:postgresql://127.0.0.1:55432/adm ADMINS_TEST_USER=postgres ADMINS_TEST_PASSWORD=x
  * Expects rows 'legacy' (sha1 of "secret") and 'modern' (bcrypt of "pässword-secret").
  */
class AdminsDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val url = sys.env.get("ADMINS_TEST_URL")

  private lazy val dbConfig = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "slick.jdbc.PostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("ADMINS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("ADMINS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))

  private def await[T](f: scala.concurrent.Future[T]) = Await.result(f, 60.seconds)
  private def stored(name: String) = await(Admin.byName(dbConfig.db, name)).get.passwordHash

  test("legacy sha1 row: login works and the row is upgraded to bcrypt") {
    assume(url.isDefined)
    val provider = new AdminsProvider(dbConfig)
    val svc = new AdminsServiceImpl(dbConfig)
    assert(PasswordHash.isLegacy(stored("legacy")))
    assert(await(provider.authenticate(Credentials("legacy", "wrong"))).isEmpty)
    assert(PasswordHash.isLegacy(stored("legacy")), "failed login must not touch the row")
    val li = await(provider.authenticate(Credentials("legacy", "secret"))).get
    assert(!PasswordHash.isLegacy(stored("legacy")))
    assert(PasswordHash.verify("secret", stored("legacy")))
    // the session identity resolves to the admin, and stops resolving if the password changes
    assert(await(svc.retrieve(li)).exists(_.username == "legacy"))
    await(Admin.upgradeHash(dbConfig.db, "legacy", stored("legacy"), PasswordHash.hash("other")))
    assert(await(svc.retrieve(li)).isEmpty)
    assert(await(svc.retrieve(LoginInfo("admin", "legacy:deadbeef"))).isEmpty)
  }

  test("bcrypt row made by Go/Java verifies and is left alone") {
    assume(url.isDefined)
    val provider = new AdminsProvider(dbConfig)
    val before = stored("modern")
    assert(await(provider.authenticate(Credentials("modern", "nope"))).isEmpty)
    assert(await(provider.authenticate(Credentials("nobody", "x"))).isEmpty)
    val li = await(provider.authenticate(Credentials("modern", "pässword-secret"))).get
    assert(li.providerKey.startsWith("modern:"))
    assert(stored("modern") == before)
  }
}
