package actors

import akka.actor.{Actor, Props, Stash}
import akka.stream.scaladsl.{Merge, Source}
import com.github.nscala_time.time.Imports.DateTime
import models._
import play.api.Logger
import play.api.libs.EventSource
import play.api.libs.EventSource.{Event, EventDataExtractor, EventNameExtractor}
import play.api.libs.json.Json
import play.api.mvc.RequestHeader
import slick.jdbc.JdbcBackend
import utils.{Ask, Concur}

import scala.collection.mutable
import scala.util.Success

// Task is visible:
// - to admins
// - if any room in it matches our rooms
// Task can be acked for a room:
// - by admins
// - if that room matches our list

object WaiterActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[WaiterActor], db)

  case object Load
  case class Loaded(tasks: Iterable[StoredWaiterTask])

  case class NewTask(message: String, rooms: Iterable[String])
  case class AckTask(id: Long, room: String)
  case class UnackTask(id: Long, room: String)
  case class DeleteTask(id: Long)

  case class TaskAcked(id: Long, when: DateTime, room: String)
  case class TaskUnacked(id: Long, room: String)
  case class TaskDeleted(id: Long)

  case class Join(perm: WaiterPermissions, requestHeader: RequestHeader)

  case class GetSnapshot(perm: WaiterPermissions)
  case class Snapshot(tasks: Seq[AdaptedWaiterTask])

  // can be filtered against rooms
  // List of list of rooms (outstanding tasks)
  type HeaderRows = Iterable[Iterable[String]]

  case class HeaderSource(rows: HeaderRows, added: Option[StoredWaiterTask])

  def adaptHeaderSource(perm: WaiterPermissions, source: HeaderSource): WaiterTaskHeader = {
    val count = source.rows.count(_.exists(perm.filter))
    val msg = source.added.filter(_.matches(perm)).map(_.message).getOrElse("")
    WaiterTaskHeader(count, msg)
  }
}

trait WaiterTaskEvent {
  def filter(perm: WaiterPermissions): Boolean
  def toEvent(perm: WaiterPermissions, requestHeader: RequestHeader): Event
}

case class WaiterTaskUpdate(id: Long, content: String)
object WaiterTaskUpdate {
  implicit val format = Json.format[WaiterTaskUpdate]
}

case class WaiterTaskUpdated(inner: StoredWaiterTask) extends WaiterTaskEvent {
  override def filter(perm: WaiterPermissions): Boolean = inner.matches(perm)

  override def toEvent(perm: WaiterPermissions, requestHeader: RequestHeader): Event = {
    val c = views.html.admin.singlewaitertask(inner.adapt(perm))
    val e = WaiterTaskUpdate(inner.id, c.body)
    Event(Json.stringify(Json.toJson(e)), None, Some("waiterTaskUpdated"))
  }
}

case class WaiterTaskDeleted(l: Long) extends WaiterTaskEvent {
  override def filter(perm: WaiterPermissions): Boolean = true

  override def toEvent(perm: WaiterPermissions, requestHeader: RequestHeader): Event =
    Event(Json.stringify(Json.toJson(WaiterTaskUpdate(l, ""))), None, Some("waiterTaskUpdated"))
}

case class WaiterTaskHeader(outstanding: Int, text: String)

object WaiterTaskHeader {
  implicit val format = Json.format[WaiterTaskHeader]
  implicit val eventNameExtractor = EventNameExtractor[WaiterTaskHeader](_ => Some("waiterTaskHeader"))
  implicit val eventDataExtractor = EventDataExtractor[WaiterTaskHeader] { state =>
    Json.stringify(Json.toJson(state))
  }
}

class WaiterActor(db: JdbcBackend#DatabaseDef) extends Actor with Stash {
  val tasks = mutable.Map[Long, StoredWaiterTask]()

  import WaiterActor._

  import scala.concurrent.ExecutionContext.Implicits.global

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Load
  }

  private val (waiterOut, waiterChannel) = Concur.broadcast[WaiterTaskEvent]
  private val (headerOut, headerChannel) = Concur.broadcast[HeaderSource]

  import scala.language.implicitConversions

  private def getHeaderRows = tasks.values.map(_.unacked)

  private def getActual: HeaderSource = {
    HeaderSource(getHeaderRows, None)
  }

  override def receive: Receive = {
    case Load => {
      val f = WaiterModel.load(db)

      f.onSuccess {
        case loaded => self ! Loaded(loaded)
      }
      f.onFailure {
        case x =>
          Logger.error("failed to load", x)
          // reload in 5 seconds
          import scala.concurrent.duration._
          import scala.language.postfixOps
          context.system.scheduler.scheduleOnce(5 seconds, self, Load)
      }
    }

    case Loaded(newtasks) => {
      newtasks.foreach { task =>
        tasks.put(task.id, task)
      }
      context.become(initialized)
    }
  }

  def initialized: Receive = {
    case GetSnapshot(perm) => {
      import com.github.nscala_time.time.Imports._
      val r = tasks.values.map(_.adapt(perm)).toSeq.sortBy(_.when).reverse
      sender ! Success(Snapshot(r))
    }

    case NewTask(message, rooms) => {
      val saved = sender()
      WaiterModel.addNewTask(db, message, rooms).map { s =>
        self ! s
        s
      }.onComplete(Ask.respond(saved, _))
    }

    case task: StoredWaiterTask => {
      tasks.put(task.id, task)
      waiterChannel.offer(WaiterTaskUpdated(task))
      headerChannel.offer(HeaderSource(getHeaderRows, Some(task)))
    }

    case AckTask(id, room) =>
      WaiterModel.markDone(db, id, room).onSuccess {
        case (when) => self ! TaskAcked(id, when, room)
      }

    case TaskAcked(id, when, room) => {
      tasks.get(id).foreach { stored =>
        val newMsg = StoredWaiterTask(stored.id, stored.when, stored.message, stored.rooms, stored.acked.updated(room, when))
        tasks.put(id, newMsg)
        waiterChannel.offer(WaiterTaskUpdated(newMsg))
        headerChannel.offer(getActual)
      }
    }

    case UnackTask(id, room) => {
      WaiterModel.unmarkDone(db, id, room).onSuccess {
        case _ =>
          self ! TaskUnacked(id, room)
      }
    }

    case TaskUnacked(id, room) => {
      tasks.get(id).foreach { stored =>
        val newMsg = StoredWaiterTask(stored.id, stored.when, stored.message, stored.rooms, stored.acked - room)
        tasks.put(id, newMsg)
        waiterChannel.offer(WaiterTaskUpdated(newMsg))
        headerChannel.offer(getActual)
      }
    }

    case Join(perm, requestHeader) => {
      sender ! Success(Source.combine(
        Source.apply(tasks.values.map(WaiterTaskUpdated).toList).concat(waiterOut).filter(_.filter(perm)).map(_.toEvent(perm, requestHeader)),
        Source.single(getActual).concat(headerOut).map(adaptHeaderSource(perm, _)) via EventSource.flow
      )(Merge(_)))
    }

    case DeleteTask(id) => {
      WaiterModel.delete(db, id).onComplete {
        case _ => self ! TaskDeleted(id)
      }
    }

    case TaskDeleted(id) => {
      tasks.remove(id)
      waiterChannel.offer(WaiterTaskDeleted(id))
      headerChannel.offer(getActual)
    }
  }
}