package models


import com.github.nscala_time.time.Imports.DateTime
import slick.jdbc.JdbcBackend

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
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel._

  // TODO: move to SlickModel
  private def maybeMakeRooms(db: JdbcBackend#DatabaseDef, roomsActive: Iterable[String])(implicit ec: ExecutionContext): Future[Iterable[String]] = {
    if (roomsActive.isEmpty) {
      makeRooms(db)
    } else Future.successful(roomsActive)
  }

  private def makeRooms(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Iterable[String]] = {
    db.run(areas.map(_.name).result)
  }

  def addNewTask(db: JdbcBackend#DatabaseDef, message: String, roomsActive: Iterable[String])(implicit ec: ExecutionContext): Future[StoredWaiterTask] =
  maybeMakeRooms(db, roomsActive).flatMap { newRa =>
    val ra = newRa.mkString(",")
    val now = DateTime.now

    val q = (waiterTasks.map(x => (x.message, x.rooms)) returning(waiterTasks.map(x => (x.id)))) += (message, ra)

    db.run(q).map { lastInsertId =>
      StoredWaiterTask(lastInsertId, now, message, newRa.toSet, Map.empty)
    }
  }

  def markDone(db: JdbcBackend#DatabaseDef, id: Long, room: String)(implicit ec: ExecutionContext): Future[DateTime] = {
    val now = DateTime.now
    db.run(waiterTaskRecords += (id, room, now)).map(_ => now)
  }

  def unmarkDone(db: JdbcBackend#DatabaseDef, id: Long, room: String)(implicit ec: ExecutionContext): Future[Unit] = {
    db.run(waiterTaskRecords.filter(x => x.id === id && x.room === room).delete).map(_ => ())
  }

  def delete(db: JdbcBackend#DatabaseDef, id: Long)(implicit ec: ExecutionContext): Future[Unit] = {
    db.run(DBIO.seq(waiterTaskRecords.filter(_.id === id).delete, sqlu"""delete from WaiterTasks where ID = $id"""))
  }

  private def getAllRecords(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Map[Long, Map[String, DateTime]]] =
    db.run(waiterTaskRecords.result).map { records =>
       records.toList.groupBy(_._1).mapValues(x => x.map(y => y._2 -> y._3).toMap)
    }

  private[this] def getAllTasks(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[List[StoredWaiterTask]] = {
    db.run(waiterTasks.result).map(_.map(x => StoredWaiterTask(x._1, x._2, x._3, x._4.split(",").toSet, Map.empty))).map(_.toList)
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