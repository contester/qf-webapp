package models

import play.api.Logging
import slick.jdbc.JdbcBackend

import scala.concurrent.ExecutionContext

object ClarificationModel extends Logging {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel._

  private[this] val clarificationsByContest = Compiled((contestID: Rep[Int]) =>
    clarificationRequests.filter(_.contest === contestID).sortBy(_.arrived.desc)
  )

  val getClarificationReq = Compiled((clarificationRequestID: Rep[Long]) =>
    clarificationRequests.filter(_.id === clarificationRequestID)
  )

  val getClarificationReqShort = Compiled((id: Rep[Long]) =>
    clarificationRequests.filter(_.id === id).map(x => (x.answer, x.answered))
  )

  def getClarificationReqs(db: JdbcBackend#DatabaseDef, contestId: Int)(implicit ec: ExecutionContext) =
    db.run(clarificationsByContest(contestId).result)

  private[this] val clarificationsByContestTeam = Compiled((contestID: Rep[Int], teamID: Rep[Int]) =>
    clarificationRequests.filter(x => x.contest === contestID && x.team === teamID).sortBy(_.arrived.desc)
  )

  def getTeamClarificationReqs(db: JdbcBackend#DatabaseDef, contestId: Int, teamId: Int)(implicit ec: ExecutionContext) =
    db.run(clarificationsByContestTeam(contestId, teamId).result)
}