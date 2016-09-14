package actors

import akka.actor.Actor.Receive
import akka.actor.{Actor, Stash}
import models.{StoredWaiterTask, WaiterModel, WaiterTaskMessage}
import play.api.libs.EventSource.{EventDataExtractor, EventNameExtractor}
import play.api.libs.iteratee.Concurrent
import play.api.libs.json.Json
import slick.jdbc.JdbcBackend

import scala.collection.mutable

// Spectator has a list of rooms it cares for.
// Task goes to the room/list of rooms.
// Spectators for these rooms get events.
// Spectators can ack their rooms.

// Events:
// - Task added (id, rooms unacked, rooms acked, message)
// - Task updated (same)
// - Task deleted (id)

import com.github.nscala_time.time.Imports.DateTime

object WaiterActor {
  case object Load
  case class Loaded(tasks: List[StoredWaiterTask])

  case class NewTask(message: String, rooms: Set[String])
  case class AckTask(id: Int, rooms: Set[String])
  case class UnackTask(id: Int, rooms: Set[String])
  case class DeleteTask(id: Int)

  case class TaskAcked(id: Int, when: DateTime, rooms: Set[String])
  case class TaskDeleted(id: Int) extends WaiterTaskMessage

  object TaskDeleted {
    implicit val format = Json.format[TaskDeleted]
    implicit val eventNameExtractor = EventNameExtractor[TaskDeleted](_ => Some("waiterTaskDeleted"))
    implicit val eventDataExtractor = EventDataExtractor[TaskDeleted] { state =>
      Json.stringify(Json.toJson(state))
    }
  }
}


class WaiterActor(db: JdbcBackend#DatabaseDef) extends Actor with Stash {
  val tasks = mutable.Map[Int, StoredWaiterTask]()

  import WaiterActor._

  private val (waiterOut, waiterChannel) = Concurrent.broadcast[WaiterTaskMessage]


  override def receive: Receive = {
    case Load => WaiterModel.load(db).onSuccess {
      case loaded => self ! Loaded(loaded)
    }

    case Loaded(newtasks) => {
      newtasks.foreach { task =>
        tasks.put(task.id, task)
      }
      context.become(initialized)
    }
  }

  def initialized: Receive = {
    case NewTask(message, rooms) =>
      WaiterModel.addNewTask(db, message, rooms).map { s =>
        self ! s
        sender ! s
      }

    case task: StoredWaiterTask => {
      tasks.put(task.id, task)
      waiterChannel.push(task)
    }

    case AckTask(id, rooms) =>
      WaiterModel.markDone(db, id, rooms.head).onSuccess {
        case (when) => self ! TaskAcked(id, when, rooms)
      }

    case TaskAcked(id, when, rooms) => {
      tasks.get(id).foreach { stored =>
        val newMsg = StoredWaiterTask(stored.id, stored.when, stored.message, stored.roomsActive, stored.roomsAcked.union(rooms))
        tasks.put(id, newMsg)
        waiterChannel.push(newMsg)
      }
    }


  }
}