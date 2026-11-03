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
  /** Full names of the service schools created by addServiceTeams. */
  val JurySchoolFullName = "Contest Jury"
  val CoachSchoolFullName = "Coaches team"

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel

  // NULL columns come back as null strings.
  private def nz(s: String): String = Option(s).map(_.trim).getOrElse("")

  // Both English and Russian names have the surname first: "Last First" / "Фамилия Имя Отчество".
  private[models] def pickSurnameEn(s: String) = s.split(' ').head
  private[models] def pickSurnameRu(s: String) = s.split(' ').head

  /**
   * What goes into teams.name: "ShortName: Surname, Surname, Surname", or just the
   * surnames when the team has no custom name. Surnames never contain a colon, so
   * consumers that only want the custom name can split on the last ": ".
   */
  private[models] def composeTeamName(customName: String, surnames: Seq[String]): String = {
    val members = surnames.mkString(", ")
    if (customName.isEmpty) members else s"$customName: $members"
  }

  // (contestant1..3, school full name, school short name, team name)
  private type Row = (String, String, String, String, String, String)

  private def convertImportedTeams(pickSurname: String => String)(m: Iterable[Row]): Iterable[ImportedTeam] =
    m.map(x => (nz(x._1), nz(x._2), nz(x._3), nz(x._4), nz(x._5), nz(x._6)))
      .filter(x => x._1.nonEmpty && x._2.nonEmpty && x._3.nonEmpty)
      .groupBy(x => (x._5, x._4))
      .mapValues(_.zipWithIndex.map { case (x, i) =>
        ImportedTeam(ImportedSchool(x._5, x._4), i + 1,
          composeTeamName(x._6, Seq(x._1, x._2, x._3).map(pickSurname)))
      }).values.flatten

  private def getImportedTeamsEn(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Iterable[ImportedTeam]] = {
    db.run(
      sql"""select t.contestant1_name_en, t.contestant2_name_en, t.contestant3_name_en, u.full_name_en,
           u.short_name_en, t.team_name from universities u, teams t where u.un_id=t.un_idf AND u.is_deleted='0'
           AND t.is_deleted='0' ORDER BY u.full_name, t.team_id""".as[Row])
      .map(convertImportedTeams(pickSurnameEn))
  }

  private def getImportedTeamsRu(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Iterable[ImportedTeam]] = {
    db.run(
      sql"""select t.contestant1_name, t.contestant2_name, t.contestant3_name, u.full_name,
           u.short_name, t.team_name from universities u, teams t where u.un_id=t.un_idf AND u.is_deleted='0'
           AND t.is_deleted='0' ORDER BY u.full_name, t.team_id""".as[Row])
      .map(convertImportedTeams(pickSurnameRu))
  }

  def getImportedTeams(db: JdbcBackend#DatabaseDef, russian: Boolean)(implicit ec: ExecutionContext): Future[Iterable[ImportedTeam]] =
    if (russian) getImportedTeamsRu(db) else getImportedTeamsEn(db)

  import scala.async.Async.{async, await}


  private[this] def insertSchoolQuery(s: ImportedSchool) = {
    (SlickModel.schools.map(x => (x.shortName, x.fullName)) returning(SlickModel.schools.map(_.id))) += (s.schoolName, s.schoolFullName)
  }


  private[this] val participantsToUpdateQ = Compiled(SlickModel.participants.map(x => (x.contest, x.team, x.notRated)))

  private[this] def genPassword(): String = {
    Range.inclusive(0, 10).map(_ => Random.nextInt(10)).mkString
  }

  def populateContestsWithTeams(db: JdbcBackend#DatabaseDef, teams: Iterable[ImportedTeam], contests: Iterable[Int], notRated: Boolean)(implicit ec: ExecutionContext): Future[Unit] = async {
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

    val filteredParticipants = participantsToUpdate.filterNot(allParticipants).map(x => (x._1, x._2, notRated))

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

  /** A kind of service team (jury, coaches): its own school, team naming and default contests. */
  case class ServiceTeamKind(key: String, shortName: String, fullName: String, teamPrefix: String, defaultContests: Seq[Int]) {
    def school: ImportedSchool = ImportedSchool(shortName, fullName)
    def teams(count: Int): Seq[ImportedTeam] = (1 to count).map(i => ImportedTeam(school, i, s"$teamPrefix$i"))
  }

  val JuryTeams = ServiceTeamKind("jury", "Jury", JurySchoolFullName, "Test ", Seq(1, 2, 3, 4, 11, 12, 13, 14))
  val CoachTeams = ServiceTeamKind("coach", "Coaches", CoachSchoolFullName, "Coach", Seq(1, 2, 3, 4))
  val ServiceTeamKinds = Seq(JuryTeams, CoachTeams)

  /** Makes sure the kind has `count` teams (never removes any) taking part, unrated, in the given contests. */
  def addServiceTeams(db: JdbcBackend#DatabaseDef, kind: ServiceTeamKind, count: Int, contests: Seq[Int])(implicit ec: ExecutionContext): Future[Unit] =
    populateContestsWithTeams(db, kind.teams(count), contests, true)

  /** How many teams the kind's school has, and the school's id (None until the first teams are added). */
  def serviceTeamStatus(db: JdbcBackend#DatabaseDef, kind: ServiceTeamKind)(implicit ec: ExecutionContext): Future[(Option[Int], Int)] =
    db.run(SlickModel.schools.filter(_.fullName === kind.fullName).result.headOption).flatMap {
      case None => Future.successful((None, 0))
      case Some(school) =>
        db.run(SlickModel.teams.filter(_.school === school.id).length.result).map(n => (Some(school.id), n))
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

  case class RoomLogin(username: String, password: String, room: String)

  def roomLogins(rooms: Seq[String], letter: String): Seq[RoomLogin] =
    rooms.distinct.sorted.map(r => RoomLogin("d" + r, letter + r, r))

  /** Rooms (areas) that have at least one computer. */
  def roomsWithComputers(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Seq[String]] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    db.run((for {
      a <- SlickModel.areas
      if SlickModel.compLocations.filter(_.location === a.id).exists
    } yield a.name).result)
  }

  /** Creates or overwrites a room admin per room: spectator of the given contests, located in the room. */
  def upsertRoomAdmins(db: JdbcBackend#DatabaseDef, logins: Seq[RoomLogin], contests: Seq[Int], hash: String => String)(implicit ec: ExecutionContext): Future[Unit] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    db.run(DBIO.sequence(logins.map { l =>
      SlickModel.admins.insertOrUpdate(org.stingray.contester.dbmodel.AdminEntry(
        l.username, hash(l.password), contests.mkString(","), "", l.room, ""))
    }).transactionally).map(_ => ())
  }
}