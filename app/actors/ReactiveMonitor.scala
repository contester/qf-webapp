package org.stingray.qf.actors

import akka.actor.{Actor, ActorRef, Stash}
import models.{Contest, Problem, Submit, Team}
import org.stingray.qf.actors.ReactiveMonitor.ResetContest
import slick.jdbc.JdbcBackend

import scala.collection.mutable

object ReactiveMonitor {
  case class ResetContest(contest: Contest)
  case class ResetTeams(teams: Map[Int, Team])
  case class ResetProblems(problems: Map[String, Problem])
  case class ResetSubmits(submits: Iterable[Submit])
  case class ResetAll(contest: Contest, teams: Map[Int, Team], problems: Map[String, Problem])
  case class ResetAllSubmits(contest: Contest, teams: Map[Int, Team], problems: Map[String, Problem], submits: Iterable[Submit])

  case class UpdateSubmit(s: Submit)

  //===
}

class ReactiveMonitor(var contestData: Contest) extends Actor {
  val teamData: mutable.Map[Int, Team] = mutable.HashMap.empty
  val problemData: mutable.Map[String, Problem] = mutable.HashMap.empty


  override def receive: Receive = {
    case ResetContest(contest) =>
      contestData = contest
  }
}

object MegaMonitor {
  type MegaState = Map[Int, Contest]
}

class MegaMonitor(db: JdbcBackend#DatabaseDef) extends Actor with Stash {
  val childActors: mutable.Map[Int, ActorRef] = mutable.HashMap.empty
  val contests: mutable.Map[Int, Contest] = mutable.HashMap.empty

  override def receive: Receive = {
    case AnyStateActor.Refresh =>
  }
}