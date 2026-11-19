package models

import org.postgresql.PGConnection
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Disqualified, NotRated}

import java.sql.{Connection, DriverManager}
import scala.concurrent.ExecutionContext

/**
  * The admin participants list and the unrated toggle on a fixture round of its own: the column is written and
  * teacher-web's trigger announces `credential_changed` "<round>:<participant>". Skipped unless QF_TEST_URL is set.
  */
class RoundParticipantsDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fx = new ContestFixture("Round participants spec")

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import fx.{await, exec, one}

  private def listening[T](channel: String)(body: Connection => T): T = {
    val conn = DriverManager.getConnection(fx.url.get, fx.user, fx.password)
    try {
      conn.createStatement().execute(s"LISTEN $channel")
      body(conn)
    } finally conn.close()
  }

  private def notifications(conn: Connection, waitMs: Int): Seq[String] = {
    val pg = conn.unwrap(classOf[PGConnection])
    val first = Option(pg.getNotifications(waitMs)).map(_.toSeq).getOrElse(Nil)
    val rest = Option(pg.getNotifications(300)).map(_.toSeq).getOrElse(Nil)
    (first ++ rest).map(_.getParameter)
  }

  private def column(participant: Int): Option[String] = {
    val r = fx.round
    one(sql"select unrated::text from round_credentials where round_id = $r and participant_id = $participant".as[Option[String]])
  }

  test("every credential of the round is listed, inactive ones too, with its unrated state") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val other = fx.addParticipant(1)
      val r = fx.round
      exec(sqlu"update round_credentials set is_active = false, unrated = 'not_rated' where round_id = $r and participant_id = $other")
      val ps = await(RoundParticipants.list(fx.db, fx.round))
      assert(ps.map(p => (p.participantId, p.isActive, p.unrated)) == Seq((fx.kid, true, None), (other, false, Some(NotRated))))
      assert(ps.head.name == fx.kidName && ps.head.number == 96 && ps.head.login.endsWith("_kid") && ps.head.school.nonEmpty)
    }
  }

  test("the toggle writes each value and the trigger announces <round>:<participant> each time") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val expected = s"${fx.round}:${fx.kid}"
      for ((value, db) <- Seq(Some(NotRated) -> Some("not_rated"), Some(Disqualified) -> Some("disqualified"), None -> None)) {
        listening("credential_changed") { conn =>
          assert(await(RoundParticipants.setUnrated(fx.db, fx.round, fx.kid, value)) == 1)
          assert(notifications(conn, 2000) == Seq(expected), s"after $value")
        }
        assert(column(fx.kid) == db)
      }
    }
  }

  test("a participant without a credential for the round, or another round, writes nothing") {
    assume(fx.url.isDefined)
    fx.withFixture {
      assert(await(RoundParticipants.setUnrated(fx.db, fx.round, Int.MaxValue, Some(NotRated))) == 0)
      assert(await(RoundParticipants.setUnrated(fx.db, fx.nullRound, fx.kid, Some(NotRated))) == 0)
      assert(column(fx.kid).isEmpty)
    }
  }
}
