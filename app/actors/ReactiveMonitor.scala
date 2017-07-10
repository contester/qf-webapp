package org.stingray.qf.actors

import akka.actor.{Actor, ActorRef, PoisonPill, Props, Stash}
import models._
import slick.jdbc.JdbcBackend

import scala.collection.mutable
import scala.concurrent.Future

object ReactiveMonitor {
  case class ResetTeams(teams: Map[Int, Team])
  case class ResetProblems(problems: Map[String, Problem])
  case class ResetSubmits(submits: Iterable[Submit])
  case class ResetAll(contest: Contest, teams: Map[Int, Team], problems: Map[String, Problem])
  case class ResetAllSubmits(contest: Contest, teams: Map[Int, Team], problems: Map[String, Problem], submits: Iterable[Submit])

  case class UpdateSubmit(s: Submit)

  //===
  def props(contestData: Contest) = Props(new ReactiveMonitor(contestData))
}

class ReactiveMonitor(var contestData: Contest) extends Actor {
  val teamData: mutable.Map[Int, Team] = mutable.HashMap.empty
  val problemData: mutable.Map[String, Problem] = mutable.HashMap.empty

  override def receive: Receive = {
    case contest: Contest =>
      contestData = contest
  }
}

object MegaMonitor {
  type MegaState = Seq[Contest]

  final case class ContestWithActor(contest: Contest, actor: ActorRef)
}

class MegaMonitor(db: JdbcBackend#DatabaseDef) extends AnyStateActor[MegaMonitor.MegaState] {
  import MegaMonitor._
  var contests: Map[Int, ContestWithActor] = Map.empty

  override def loadStart(): Future[MegaState] = {
    db.run(Contests.getContests)
  }

  private def updatedContest(contest: Contest) =
    contests.get(contest.id) match {
      case Some(other) =>
        if (other.contest != contest) {
          other.actor ! contest
          ContestWithActor(contest, other.actor)
        } else {
          other
        }
      case None =>
        ContestWithActor(contest, context.actorOf(ReactiveMonitor.props(contest)))
    }


  override def setState(v: MegaState): Unit = {
    val removed = contests -- v.map(_.id)
    val updated = v.map(updatedContest).map(x => x.contest.id -> x).toMap
    removed.values.foreach(_.actor ! PoisonPill)
    contests = updated
  }

  override def initialized: Receive = {
    case contest: Contest =>
      val updated = updatedContest(contest)
      contests = contests.updated(contest.id, updated)
  }
}