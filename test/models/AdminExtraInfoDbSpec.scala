package models

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Extrainfo
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/**
  * Add, edit, renumber and delete of extra pages. This WRITES, so it is skipped unless both ADMINS_TEST_URL (same
  * variables as AdminSubmitsDbSpec) and EXTRAINFO_TEST_CONTEST (a throwaway contest id) are set. It only touches
  * pages numbered 900001 and up, and removes them at the end.
  */
class AdminExtraInfoDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val url = sys.env.get("ADMINS_TEST_URL")
  private val contest = sys.env.get("EXTRAINFO_TEST_CONTEST").map(_.toInt)

  private lazy val dbConfig = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("ADMINS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("ADMINS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))

  private def await[T](f: Future[T]) = Await.result(f, 60.seconds)
  private lazy val db = dbConfig.db

  private val A = 900001
  private val B = 900002
  private val C = 900003

  private def mine = await(AdminExtraInfo.list(db, contest.get)).filter(_.num >= A)

  test("add, edit, renumber, clash, delete") {
    assume(url.isDefined && contest.isDefined)
    val c = contest.get
    try {
      assert(await(AdminExtraInfo.save(db, c, None, Extrainfo(0, A, "one", "<p>1</p>"))) == AdminExtraInfo.Saved)
      assert(await(AdminExtraInfo.save(db, c, None, Extrainfo(0, B, "two", "<p>2</p>"))) == AdminExtraInfo.Saved)
      assert(mine.map(_.num) == Seq(A, B), "listed in number order")
      assert(mine.forall(_.contest == c))

      assert(await(AdminExtraInfo.save(db, c, None, Extrainfo(0, A, "dup", ""))) == AdminExtraInfo.NumberTaken)
      assert(await(AdminExtraInfo.get(db, c, A)).get.heading == "one", "a clash must not overwrite")

      // edit in place, number unchanged
      assert(await(AdminExtraInfo.save(db, c, Some(A), Extrainfo(0, A, "one!", "<p>1!</p>"))) == AdminExtraInfo.Saved)
      assert(await(AdminExtraInfo.get(db, c, A)).get.data == "<p>1!</p>")

      // renumber A -> C, B untouched
      assert(await(AdminExtraInfo.save(db, c, Some(A), Extrainfo(0, C, "one!", "<p>1!</p>"))) == AdminExtraInfo.Saved)
      assert(await(AdminExtraInfo.get(db, c, A)).isEmpty)
      assert(mine.map(_.num) == Seq(B, C))
      assert(await(AdminExtraInfo.get(db, c, B)).get.heading == "two")

      // renumber onto an existing page is refused and changes nothing
      assert(await(AdminExtraInfo.save(db, c, Some(C), Extrainfo(0, B, "x", "x"))) == AdminExtraInfo.NumberTaken)
      assert(mine.map(x => (x.num, x.heading)) == Seq((B, "two"), (C, "one!")))

      // editing a page that is gone
      assert(await(AdminExtraInfo.save(db, c, Some(A), Extrainfo(0, A, "ghost", ""))) == AdminExtraInfo.Missing)

      assert(await(AdminExtraInfo.delete(db, c, B)))
      assert(!await(AdminExtraInfo.delete(db, c, B)))
      assert(mine.map(_.num) == Seq(C))
    } finally {
      Seq(A, B, C).foreach(n => await(AdminExtraInfo.delete(db, contest.get, n)))
    }
    assert(mine.isEmpty)
  }
}
