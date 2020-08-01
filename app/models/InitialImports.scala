package models

import com.github.tminglei.slickpg.InetString
import inet.ipaddr.IPAddressString
import slick.basic.DatabaseConfig
import slick.jdbc.{JdbcBackend, JdbcProfile}
import slick.sql.SqlStreamingAction

import scala.concurrent.{ExecutionContext, Future}

case class ExternalDatabases(studentWeb: DatabaseConfig[JdbcProfile], schoolWeb: DatabaseConfig[JdbcProfile], netmap: DatabaseConfig[JdbcProfile], pwlist: Seq[String])
case class ImportedSchool(schoolName: String, schoolFullName: String)
case class ImportedTeam(school: ImportedSchool, teamID: Int, teamName: String)

case class ImportedComputer(location: String, addr: String, name: String)

object InitialImportTools {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel

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

  private[this] def insertSchoolQuery(s: ImportedSchool) = {
    (SlickModel.schools.map(x => (x.name, x.fullName)) returning(SlickModel.schools.map(_.id))) += (s.schoolName, s.schoolFullName)
  }

  private def addParticipant(db: JdbcBackend#DatabaseDef, teamID: Int, contestID: Int)(implicit ec: ExecutionContext) = {
    db.run(SlickModel.participants.filter(x => (x.contest === contestID && x.team === teamID)).result.headOption.flatMap {
      case Some(id) => DBIO.successful(id)
      case None => SlickModel.participants.map(x => (x.contest, x.team)).insertOrUpdate((contestID, teamID))
    })
  }

  def populateContestsWithTeams(db: JdbcBackend#DatabaseDef, teams: Iterable[ImportedTeam], contests: Iterable[Int], passwords: Seq[String])(implicit ec: ExecutionContext): Future[Unit] = async {
    val currentSchools = await(db.run(SlickModel.schools.result))
    val schoolLookups = currentSchools.map(x => (x.name -> x.id)).toMap
    val importedSchools = teams.map(_.school).toSet
    val foundSchools = importedSchools.map(x => x -> schoolLookups.get(x.schoolName)).toMap
    val addedSchools = await(Future.sequence(foundSchools.filter(_._2.isEmpty).keySet.map { ns =>
      db.run(insertSchoolQuery(ns)).map(x => (ns, x))
    }))

    val mergedSchools = (foundSchools.filter(_._2.isDefined).mapValues(_.get).toSeq ++ addedSchools).toMap

    val teamIDs = await(db.run(DBIO.sequence(teams.map { team =>
      val schoolID = mergedSchools.get(team.school).get
      val schoolQuery = SlickModel.teams.filter(x => (x.school === schoolID && x.num === team.teamID)).map(x => (x.id, x.name)).take(1).result.headOption.flatMap {
        case Some(x) =>
          if (x._2 == team.teamName) {
            DBIO.successful(x._1)
          } else {
            SlickModel.teams.filter(_.id === x._1).map(_.name).update(team.teamName).map(_ => x._1)
          }
        case None =>
          (SlickModel.teams.map(x => (x.school, x.num, x.name)) returning (SlickModel.teams.map(_.id))) += (schoolID, team.teamID, team.teamName)
      }
      schoolQuery
    })))

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

    await(db.run(DBIO.sequence(assignmentsWithPassword.map { a =>
      SlickModel.assignments.filter(x => (x.contest === a._1 && x.team === a._2)).map(_.password).result.headOption.flatMap {
        case Some(pwd) =>
          if (pwd == "") {
            SlickModel.assignments.filter(x => (x.contest === a._1 && x.team === a._2)).map(x => (x.username, x.password)).update(a._3, a._4)
          } else DBIO.successful(1)
        case None =>
          SlickModel.assignments.map(x => (x.contest, x.team, x.username, x.password)) += a
      }
    })))
  }

  def getNetmapComputers(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext) =
    db.run(sql"select l.room, inet_ntoa(l.ip), h.name from contest_locations l, unetmap_host h where l.ip = h.ip order by l.room, h.name".as[(String, String, String)]).map(_.map(x => ImportedComputer(x._1, x._2, x._3)))

  def importNetmapComputers(db: JdbcBackend#DatabaseDef, comps: Seq[ImportedComputer])(implicit ec: ExecutionContext) = async {
    val roomSet = comps.map(_.location).toSet
    val existingRooms = await(db.run(SlickModel.areas.map(x => (x.name, x.id)).result)).toMap
    val newRooms = roomSet -- existingRooms.keySet

    val addedRooms = await(db.run(DBIO.sequence(newRooms.toSeq.map { room =>
      (SlickModel.areas.map(_.name) returning(SlickModel.areas.map(_.id)) into ((id, name) => id -> name)+= (room))
    }))).toMap

    val combinedRooms = addedRooms ++ existingRooms
    db.run(DBIO.sequence(comps.map { comp =>
      val roomID = combinedRooms.get(comp.location).get
      SlickModel.compLocations.insertOrUpdate(SlickModel.CompLocation(InetString(comp.addr), roomID, comp.name))
    }))
  }
}