package models

import slick.basic.DatabaseConfig
import slick.jdbc.{JdbcBackend, JdbcProfile}
import slick.sql.SqlStreamingAction

import scala.concurrent.{ExecutionContext, Future}

case class ExternalDatabases(studentWeb: DatabaseConfig[JdbcProfile], schoolWeb: DatabaseConfig[JdbcProfile], netmap: DatabaseConfig[JdbcProfile])
case class ImportedSchool(schoolName: String, schoolFullName: String)
case class ImportedTeam(school: ImportedSchool, teamID: Int, teamName: String)

case class ImportedComputer(location: String, addr: String, name: String)

object InitialImportTools {
  import slick.jdbc.MySQLProfile.api._
  import utils.Db._
  import com.github.tototoshi.slick.MySQLJodaSupport._

  private def pickName(s: String) =
    s.split(' ').head

  private def pickNames(n1: String, n2: String, n3: String) =
    s"${pickName(n1)}, ${pickName(n2)}, ${pickName(n3)}"

  def getImportedTeamsEn(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Iterable[ImportedTeam]] = {
    db.run(
      sql"""select t.contestant1_name_en, t.contestant2_name_en, t.contestant3_name_en, u.full_name_en,
           u.short_name_en from universities u, teams t where u.un_id=t.un_idf AND u.is_deleted='0'
           AND t.is_deleted='0' ORDER BY u.full_name, t.team_id""".as[(String, String, String, String, String)]).map(convertImportedTeams(_))
  }

  def getImportedTeamsRu(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Iterable[ImportedTeam]] = {
    db.run(
      sql"""select t.contestant1_name, t.contestant2_name, t.contestant3_name, u.full_name,
           u.short_name from universities u, teams t where u.un_id=t.un_idf AND u.is_deleted='0'
           AND t.is_deleted='0' ORDER BY u.full_name, t.team_id""".as[(String, String, String, String, String)]).map(convertImportedTeams(_))
  }
  private def convertImportedTeams(m: Iterable[(String, String, String, String, String)]): Iterable[ImportedTeam] =
    m.groupBy(x => (x._5, x._4)).mapValues(_.zipWithIndex.map(x => ImportedTeam(ImportedSchool(x._1._5, x._1._4), x._2+1, pickNames(x._1._1, x._1._2, x._1._3)))).values.flatten

  import scala.async.Async.{async, await}

  private val selLastID = sql"select LAST_INSERT_ID()".as[Int].head

  private def insertSchoolQuery(s: ImportedSchool) =
    sqlu"insert into Schools (Name, FullName) values (${s.schoolName}, ${s.schoolFullName})".andThen(selLastID).withPinnedSession

  private def addParticipant(db: JdbcBackend#DatabaseDef, teamID: Int, contestID: Int)(implicit ec: ExecutionContext) = async {
    val foundAssignment = await(db.run(sql"select LocalID from Participants where Contest = $contestID and Team = $teamID".as[Int].headOption))
    foundAssignment match {
      case Some(id) => ()
      case None => await(db.run(sqlu"insert into Participants (Contest, Team, LocalID) values ($contestID, $teamID, $teamID)").map(x => ()))
    }
  }

  def populateContestsWithTeams(db: JdbcBackend#DatabaseDef, teams: Iterable[ImportedTeam], contests: Iterable[Int], passwords: Seq[String])(implicit ec: ExecutionContext): Future[Unit] = async {
    val currentSchools = await(db.run(sql"select ID, Name, FullName from Schools".as[(Int, String, String)]))
    val schoolLookups = currentSchools.map(x => (x._2 -> x._1)).toMap
    val importedSchools = teams.map(_.school).toSet
    val foundSchools = importedSchools.map(x => x -> schoolLookups.get(x.schoolName)).toMap
    val addedSchools = await(Future.sequence(foundSchools.filter(_._2.isEmpty).keySet.map { ns =>
      db.run(insertSchoolQuery(ns)).map(x => (ns, x))
    }))

    val mergedSchools = (foundSchools.filter(_._2.isDefined).mapValues(_.get).toSeq ++ addedSchools).toMap

    val teamIDs = await(Future.sequence(teams.map { team =>
      val schoolID = mergedSchools.get(team.school)
      db.run(sql"select ID, Name from Teams where School = $schoolID and Num = ${team.teamID}".as[(Int, String)].headOption).flatMap { zx =>
        zx match {
          case Some(x) =>
            if (x._2 == team.teamName) {
              Future.successful(x._1)
            } else {
              db.run(sqlu"update Teams set Name = ${team.teamName} where ID = ${x._1}").map(_ => x._1)
            }
          case None =>
            db.run(sqlu"insert into Teams (School, Num, Name) values ($schoolID, ${team.teamID}, ${team.teamName})".andThen(selLastID).withPinnedSession)
        }
      }}))

    val participantsToUpdate = for {
      c <- contests
      t <- teamIDs
    } yield (c, t)

    val assignmentsToUpdate = for {
      c <- contests
      t <- teamIDs
    } yield (c, t, s"team${c}_${t}")

    val assignmentsWithPassword = assignmentsToUpdate.zip(passwords).map(x => (x._1._1, x._1._2, x._1._3, x._2))

    await(Future.sequence(participantsToUpdate.map { p =>
      addParticipant(db, p._2, p._1)
    }))

    await(Future.sequence(assignmentsWithPassword.map { a =>
      db.run(sql"select Password from Assignments where Contest = ${a._1} and LocalID = ${a._2}".as[String].headOption).flatMap { xp =>
        xp match {
          case Some(pwd) =>
            if (pwd == "") {
              db.run(sqlu"replace into Assignments (Contest, LocalID, Username, Password) values (${a._1}, ${a._2}, ${a._3}, ${a._4})")
            } else Future.successful(1)
          case None =>
            db.run(sqlu"replace into Assignments (Contest, LocalID, Username, Password) values (${a._1}, ${a._2}, ${a._3}, ${a._4})")
        }
      }
    }))
  }

  def getNetmapComputers(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext) =
    db.run(sql"select l.room, inet_ntoa(l.ip), h.name from contest_locations l, unetmap_host h where l.ip = h.ip order by l.room, h.name".as[(String, String, String)]).map(_.map(x => ImportedComputer(x._1, x._2, x._3)))

  def importNetmapComputers(db: JdbcBackend#DatabaseDef, comps: Seq[ImportedComputer])(implicit ec: ExecutionContext) = async {
    val roomSet = comps.map(_.location).toSet
    val existingRooms = await(db.run(sql"select Name, ID from Areas".as[(String, Int)])).toMap
    val newRooms = roomSet -- existingRooms.keySet

    val addedRooms = await(Future.sequence(newRooms.toSeq.map { room =>
      db.run(sqlu"insert into Areas (Name) values ($room)".andThen(selLastID).withPinnedSession).map(room -> _)
    })).toMap

    val combinedRooms = addedRooms ++ existingRooms

    Future.sequence(comps.map { comp =>
      val roomID = combinedRooms.get(comp.location).get
      db.run(sqlu"replace CompLocations (ID, Location, Name) values (inet_aton(${comp.addr}), $roomID, ${comp.name})")
    })
  }
}