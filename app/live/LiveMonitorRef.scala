package live

import akka.actor.ActorRef
import org.stingray.contester.dbmodel.Contest

import scala.concurrent.Future

/** The live monitor actor for the controllers; None when the live monitor is disabled. A wrapper so macwire can
 * tell it apart from other optional actor refs. */
case class LiveMonitorRef(ref: Option[ActorRef])

/** Looks up a contest by id for the public page shell (name, times). */
case class LiveContestLookup(get: Int => Future[Option[Contest]])
