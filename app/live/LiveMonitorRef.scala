package live

import akka.actor.ActorRef

/** The admin live monitor actor for the controllers (None only in tests). A wrapper so macwire can tell it apart from
 * other optional actor refs. */
case class LiveMonitorRef(ref: Option[ActorRef])
