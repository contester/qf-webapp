package models

import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.SlickModel

import scala.concurrent.ExecutionContext

/** A message ack marks only the acknowledging participant's own message seen. Skipped unless QF_TEST_URL is set. */
class MessageAckDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fx = new ContestFixture("Message ack spec")

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import fx.{exec, one}

  test("an ack of another participant's message id changes nothing; the owner's ack marks it seen") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val (s, r, k) = (fx.season, fx.round, fx.kid)
      val other = fx.addParticipant(1)
      val msg = one(sql"insert into messages (season_id, round_id, participant_id, kind, value, seen) values ($s, $r, $k, 'k', '{}', false) returning id".as[Long])
      def seen = one(sql"select seen from messages where id = $msg".as[Boolean])
      assert(fx.await(fx.db.run(SlickModel.messageSeenByID((msg, r, other)).update(true))) == 0)
      assert(fx.await(fx.db.run(SlickModel.messageSeenByID((msg, fx.nullRound, k)).update(true))) == 0)
      assert(!seen)
      assert(fx.await(fx.db.run(SlickModel.messageSeenByID((msg, r, k)).update(true))) == 1)
      assert(seen)
    }
  }
}
