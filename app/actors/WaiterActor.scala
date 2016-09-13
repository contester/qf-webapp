package actors

import akka.actor.Actor
import models.StoredWaiterTask
import slick.jdbc.JdbcBackend

// Spectator has a list of rooms it cares for.
// Task goes to the room/list of rooms.
// Spectators for these rooms get events.
// Spectators can ack their rooms.

// Events:
// - Task added (id, rooms unacked, rooms acked, message)
// - Task updated (same)
// - Task deleted (id)

object WaiterActor {
}


class WaiterActor(db: JdbcBackend#DatabaseDef) extends Actor {
  val tasks = Map[Int, StoredWaiterTask]()



  override def receive: Receive = ???
}