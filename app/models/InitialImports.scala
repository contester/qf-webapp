package models

import com.github.tminglei.slickpg.InetString
import inet.ipaddr.IPAddressString
import slick.basic.DatabaseConfig
import slick.jdbc.{JdbcBackend, JdbcProfile}
import slick.sql.SqlStreamingAction

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

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

  private def getImportedTeamsEn(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Iterable[ImportedTeam]] = {
    db.run(
      sql"""select t.contestant1_name_en, t.contestant2_name_en, t.contestant3_name_en, u.full_name_en,
           u.short_name_en from universities u, teams t where u.un_id=t.un_idf AND u.is_deleted='0'
           AND t.is_deleted='0' ORDER BY u.full_name, t.team_id""".as[(String, String, String, String, String)])
      .map(convertImportedTeams(_))
  }

  private def getImportedTeamsRu(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Iterable[ImportedTeam]] = {
    db.run(
      sql"""select t.contestant1_name, t.contestant2_name, t.contestant3_name, u.full_name,
           u.short_name from universities u, teams t where u.un_id=t.un_idf AND u.is_deleted='0'
           AND t.is_deleted='0' ORDER BY u.full_name, t.team_id""".as[(String, String, String, String, String)]).map(convertImportedTeams(_))
  }

  def getImportedTeams(db: JdbcBackend#DatabaseDef, russian: Boolean)(implicit ec: ExecutionContext): Future[Iterable[ImportedTeam]] = {
    if (russian) {
      getImportedTeamsRu(db)
    } else {
      getImportedTeamsEn(db)
    }
  }
  private def convertImportedTeams(m: Iterable[(String, String, String, String, String)]): Iterable[ImportedTeam] =
    m.filter(x => (!x._1.isBlank && !x._2.isBlank && !x._3.isBlank))
      .groupBy(x => (x._5, x._4))
      .mapValues(_.zipWithIndex.map(x => ImportedTeam(ImportedSchool(x._1._5, x._1._4), x._2+1, pickNames(x._1._1, x._1._2, x._1._3)))).values.flatten

  import scala.async.Async.{async, await}


  private[this] def insertSchoolQuery(s: ImportedSchool) = {
    (SlickModel.schools.map(x => (x.shortName, x.fullName)) returning(SlickModel.schools.map(_.id))) += (s.schoolName, s.schoolFullName)
  }


  private[this] val participantsToUpdateQ = Compiled(SlickModel.participants.map(x => (x.contest, x.team)))

  private[this] def genPassword(): String = {
    Range.inclusive(0, 10).map(_ => Random.nextInt(10)).mkString
  }

  def populateContestsWithTeams(db: JdbcBackend#DatabaseDef, teams: Iterable[ImportedTeam], contests: Iterable[Int])(implicit ec: ExecutionContext): Future[Unit] = async {
    // Import missing schools and create mapping schoolname -> id
    val currentSchools = await(db.run(SlickModel.schools.result))
    val schoolLookups = currentSchools.map(x => (x.fullName -> x.id)).toMap
    val importedSchools = teams.map(_.school).toSet
    val foundSchools = importedSchools.map(x => x -> schoolLookups.get(x.schoolFullName)).toMap
    val addedSchools = await(Future.sequence(foundSchools.filter(_._2.isEmpty).keySet.map { ns =>
      db.run(insertSchoolQuery(ns)).map(x => (ns, x))
    }))

    val mergedSchools = (foundSchools.filter(_._2.isDefined).mapValues(_.get).toSeq ++ addedSchools).toMap

    val currentTeams = await(db.run(SlickModel.teams.result))
    val teamLookups = currentTeams.map(x => ((x.school, x.num) -> x)).toMap


    val teamIDs = await(db.run(DBIO.sequence(teams.map { team =>
      val schoolID = mergedSchools(team.school)

      teamLookups.get((schoolID, team.teamID)) match {
        case Some(value) =>
          if (value.name == team.teamName) DBIO.successful(value.id)
          else SlickModel.teams.filter(_.id === value.id).map(_.name).update(team.teamName).map(_ => value.id)
        case None =>
          (SlickModel.teams.map(x => (x.school, x.num, x.name)) returning (SlickModel.teams.map(_.id))) += (schoolID, team.teamID, team.teamName)
      }
    })))

    val allParticipants = await(db.run(SlickModel.participants.result)).map(x => x.contest -> x.team).toSet
    val participantsToUpdate = for {
      c <- contests
      t <- teamIDs
    } yield (c, t)

    val filteredParticipants = participantsToUpdate.filterNot(allParticipants)

    await(db.run(participantsToUpdateQ ++= filteredParticipants))

    val assignmentsWith = (for {
      c <- contests
      t <- teamIDs
    } yield (c, t, s"team${c}_${t}")).toSet.toSeq

    val allAssignments = await(db.run(SlickModel.assignments.result)).map(x => (x.username -> x)).toMap

    val fresq = assignmentsWith.flatMap { one =>
      allAssignments.get(one._3) match {
        case Some(value) => if (value.contest == one._1 && value.teamId == one._2) None
                else Some(SlickModel.assignments.filter(_.username === one._3).map(x => (x.contest, x.team)).update(one._1, one._2))
        case None =>
          Some(SlickModel.assignments.map(x => (x.contest, x.team, x.username, x.password)) += (one._1, one._2, one._3, genPassword()))
      }
    }

    await(db.run(DBIO.sequence(fresq)))
  }

  def addJuryTeams(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext) = {
    val jurySchool = ImportedSchool("Jury", "Contest Jury")
    val teams = Seq(ImportedTeam(jurySchool, 1, "Test 1"), ImportedTeam(jurySchool, 2, "Test 2"), ImportedTeam(jurySchool, 3, "Test 3"))

    populateContestsWithTeams(db, teams, Seq(1,2,3,4,11,12,13,14))
  }

  def getNetmapComputers(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext) =
    db.run(sql"select l.room, inet_ntoa(h.ip), h.name from contest_locations l, unetmap_host h where l.id = h.id order by l.room, h.name".as[(String, String, String)]).map(_.map(x => ImportedComputer(x._1, x._2, x._3)))

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