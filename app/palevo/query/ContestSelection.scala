package palevo.query

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

/** How the standings of a contest relate to the freeze, as reported in the `view` field. */
sealed trait View { def json: String }
case object Frozen extends View { val json = "frozen" }
case object LiveJury extends View { val json = "live-jury" }
case object NoFreeze extends View { val json = "no-freeze" }

/** Pure contest selection and freeze logic; `now` is always a parameter. */
object ContestSelection {
  /** Same rule as `FreezeRules.hides`, evaluated for `now` itself. */
  def freezeInEffect(c: Contest, now: DateTime): Boolean =
    c.freezeTime.isBefore(c.endTime) && !now.isBefore(c.freezeTime) && now.isBefore(c.exposeTime)

  def viewOf(c: Contest, a: Audience, now: DateTime): View =
    if (!freezeInEffect(c, now)) NoFreeze
    else a match {
      case Public => Frozen
      case Jury => LiveJury
    }

  def status(c: Contest, now: DateTime): String =
    if (now.isBefore(c.startTime)) "before"
    else if (now.isBefore(c.endTime)) "running"
    else "over"

  def select(all: Seq[Contest], scope: Scope, allowed: Set[Int], now: DateTime,
             explicit: Option[Int]): Either[String, Seq[Contest]] = {
    val candidates = all.filter(c => allowed(c.id) && scope.contests.forall(_.contains(c.id)))
    explicit match {
      case Some(id) =>
        candidates.find(_.id == id).map(c => Right(Seq(c)))
          .getOrElse(Left(s"contest $id is not available in this scope"))
      case None =>
        val running = candidates.filter(status(_, now) == "running").sortBy(_.id)
        if (running.nonEmpty) Right(running)
        else {
          val finished = candidates.filter(status(_, now) == "over")
          if (finished.nonEmpty)
            Right(Seq(finished.maxBy(c => (c.endTime.getMillis, c.id))))
          else
            Right(candidates.sortBy(c => (c.startTime.getMillis, c.id)).take(1))
        }
    }
  }
}
