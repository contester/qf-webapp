package models


import com.github.nscala_time.time.Imports.DateTime
import play.api.Logger
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{ExecutionContext, Future}

// Model: stored things.

case class StoredWaiterTask(id: Long, when: DateTime, message: String, rooms: Set[String],
                            acked: Map[String, DateTime]) {
  def matches(perm: WaiterPermissions): Boolean =
    perm.canCreateTasks || rooms.exists(perm.filter)

  def adapt(perm: WaiterPermissions): AdaptedWaiterTask = {
    val odmin = perm.canCreateTasks
    val ac = acked.keys.map(x => RoomWithPermission(x, odmin || perm.filter(x))).toSeq.sortBy(x => (!x.can, x.name))
    val un = rooms.filterNot(acked.contains).map(x => RoomWithPermission(x, odmin || perm.filter(x))).toSeq.sortBy(x => (!x.can, x.name))
    AdaptedWaiterTask(id, when, message, un, ac, odmin)
  }

  def unacked = rooms -- acked.keys
}

case class RoomWithPermission(name: String, can: Boolean)

case class AdaptedWaiterTask(id: Long, when: DateTime, message: String, unacked: Seq[RoomWithPermission],
                             acked: Seq[RoomWithPermission], canDelete: Boolean)

object WaiterModel {
  import slick.jdbc.MySQLProfile.api._
  import utils.Db._

  case class WaiterTaskRecord(id: Long, room: String, ts: DateTime)

  class WaiterTaskRecords(tag: Tag) extends Table[WaiterTaskRecord](tag, "WaiterTasksRecord") {
    def id = column[Long]("ID")
    def room = column[String]("Room")
    def ts = column[DateTime]("TS")

    override def * = (id, room, ts) <> (WaiterTaskRecord.tupled, WaiterTaskRecord.unapply)
  }

  val waiterTaskRecords = TableQuery[WaiterTaskRecords]

  private def maybeMakeRooms(db: JdbcBackend#DatabaseDef, roomsActive: Iterable[String])(implicit ec: ExecutionContext): Future[Iterable[String]] = {
    if (roomsActive.isEmpty) {
      makeRooms(db)
    } else Future.successful(roomsActive)
  }

  private def makeRooms(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Iterable[String]] = {
    db.run(sql"""select Name from Areas""".as[String]).map(_.toList)
  }

  def addNewTask(db: JdbcBackend#DatabaseDef, message: String, roomsActive: Iterable[String])(implicit ec: ExecutionContext): Future[StoredWaiterTask] =
  maybeMakeRooms(db, roomsActive).flatMap { newRa =>
    val ra = newRa.mkString(",")
    val now = DateTime.now
    db.run(sqlu"""insert into WaiterTasks (TS, Message, Rooms) values ($now, $message, $ra)
                  """.andThen(sql"select last_insert_id()".as[Int]).withPinnedSession).map(_.headOption.get).map { lastInsertId =>
      StoredWaiterTask(lastInsertId, now, message, newRa.toSet, Map.empty)
    }
  }

  def markDone(db: JdbcBackend#DatabaseDef, id: Long, room: String)(implicit ec: ExecutionContext): Future[DateTime] = {
    val now = DateTime.now
    db.run(waiterTaskRecords += WaiterTaskRecord(id, room, now)).map(_ => now)
  }

  def unmarkDone(db: JdbcBackend#DatabaseDef, id: Long, room: String)(implicit ec: ExecutionContext): Future[Unit] = {
    db.run(waiterTaskRecords.filter(x => x.id === id && x.room === room).delete).map(_ => ())
  }

  def delete(db: JdbcBackend#DatabaseDef, id: Long)(implicit ec: ExecutionContext): Future[Unit] = {
    db.run(DBIO.seq(waiterTaskRecords.filter(_.id === id).delete, sqlu"""delete from WaiterTasks where ID = $id"""))
  }

/*  def update(db: JdbcBackend#DatabaseDef, id: Long, message: String, rooms: Set[String], reset: Boolean)(implicit ec: ExecutionContext) =
    maybeMakeRooms(db, rooms)*/

  private def getAllRecords(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Map[Long, Map[String, DateTime]]] =
    db.run(waiterTaskRecords.result).map { records =>
      records.toList.groupBy(_.id).mapValues(x => x.map(y => y.room -> y.ts).toMap)
    }

  implicit private val getStoredTask = GetResult(r =>
    StoredWaiterTask(r.nextLong(), new DateTime(r.nextTimestamp()), r.nextString(), r.nextString().split(",").toSet, Map.empty)
  )

  private def getAllTasks(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[List[StoredWaiterTask]] = {
    db.run(sql"""select ID, TS, Message, Rooms from WaiterTasks""".as[StoredWaiterTask]).map(_.toList)
  }

  def load(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[List[StoredWaiterTask]] = {
    getAllRecords(db).zip(getAllTasks(db)).map {
      case (records, tasks) =>
        tasks.map {
          case StoredWaiterTask(id, ts, message, roomsActive, _) =>
            StoredWaiterTask(id, ts, message, roomsActive, records.getOrElse(id, Map.empty))
        }
    }
  }
}