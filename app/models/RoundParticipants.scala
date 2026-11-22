package models

import org.stingray.contester.dbmodel.{Disqualified, NotRated, SlickModel, Unrated}
import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}

/** A participant of a round as the admin participants page lists it: every credential, active or not. */
case class RoundParticipant(participantId: Int, number: Int, name: String, school: String, city: String, login: String,
                            isActive: Boolean, unrated: Option[Unrated])

/** The values of the unrated toggle: `none` (rated, NULL), `not_rated`, `disqualified`. */
object UnratedChoice {
  val all: Seq[Option[Unrated]] = Seq(None, Some(NotRated), Some(Disqualified))

  def value(u: Option[Unrated]): String = u.fold("none")(Unrated.toDb)

  /** None: not one of the three values. */
  def parse(s: String): Option[Option[Unrated]] = all.find(value(_) == s)
}

object RoundParticipants {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  /** The round's credentials with their participants (soft-deleted participants left out), by number. */
  def list(db: JdbcBackend#DatabaseDef, round: Int)(implicit ec: ExecutionContext): Future[Seq[RoundParticipant]] =
    db.run((for {
      rc <- SlickModel.roundCredentials if rc.roundId === round
      p <- SlickModel.participants if p.id === rc.participantId && !p.isDeleted
      s <- SlickModel.schools if s.id === p.schoolId
    } yield (p.id, p.seasonSeq, p.fullName, s.name, s.city, rc.login, rc.isActive, p.unrated)).sortBy(_._2).result)
      .map(_.map((RoundParticipant.apply _).tupled))

  /**
    * Writes `participants.unrated` for a participant enrolled in `round` (NULL for rated). The update
    * fires teacher-web's `participant_changed` trigger, which updates the live monitor. Returns the rows updated:
    * 0 when the participant has no credential for the round.
    */
  def setUnrated(db: JdbcBackend#DatabaseDef, round: Int, participant: Int, unrated: Option[Unrated])(implicit ec: ExecutionContext): Future[Int] =
    db.run((for {
      exists <- SlickModel.roundCredentials
        .filter(rc => rc.roundId === round && rc.participantId === participant)
        .exists.result
      updated <- if (exists)
        SlickModel.participants.filter(_.id === participant).map(_.unrated).update(unrated)
      else DBIO.successful(0)
    } yield updated).transactionally)
}
