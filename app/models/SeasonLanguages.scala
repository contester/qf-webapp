package models

import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}

case class SeasonLanguage(id: Int, name: String, moduleId: String)

/** A season that has languages, for the "copy from" choice. */
case class SeasonWithLanguages(id: Int, name: String, languages: Int)

sealed trait CopyLanguagesResult
object CopyLanguagesResult {
  case class Copied(added: Int) extends CopyLanguagesResult
  /** The source is the target: nothing to copy. */
  case object SameSeason extends CopyLanguagesResult
}

/**
  * The language list of a season (`season_languages`), shared by every round of the season. qf only lists it and
  * copies it from another season; single languages are added and removed in teacher-web or with SQL (a language
  * that submits refer to cannot be removed at all: the foreign keys are RESTRICT).
  */
object SeasonLanguages {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  def currentSeason(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Option[(Int, String)]] =
    db.run(sql"select id, name from seasons where is_current".as[(Int, String)].headOption)

  def list(db: JdbcBackend#DatabaseDef, seasonId: Int)(implicit ec: ExecutionContext): Future[Seq[SeasonLanguage]] =
    db.run(sql"select id, name, module_id from season_languages where season_id = $seasonId order by id"
      .as[(Int, String, String)]).map(_.map((SeasonLanguage.apply _).tupled))

  /** Every season that has at least one language, newest first. */
  def seasonsWithLanguages(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Seq[SeasonWithLanguages]] =
    db.run(sql"""select s.id, s.name, count(*)::int from seasons s join season_languages l on l.season_id = s.id
                  group by s.id, s.name order by s.id desc""".as[(Int, String, Int)]).map(_.map((SeasonWithLanguages.apply _).tupled))

  // pg_advisory_xact_lock(key, season): two copies into the same season at once do not both add the same rows.
  private val CopyLockKey = 0x71666c61 // "qfla"

  /**
    * Copies the languages of season `from` into season `to`, in their order, skipping every (name, module_id) that
    * `to` already has; so a second copy adds nothing. The caller passes the current season as `to`.
    */
  def copyLanguages(db: JdbcBackend#DatabaseDef, from: Int, to: Int)(implicit ec: ExecutionContext): Future[CopyLanguagesResult] =
    if (from == to) Future.successful(CopyLanguagesResult.SameSeason)
    else db.run((sql"select pg_advisory_xact_lock($CopyLockKey, $to)::text".as[String] >>
      sqlu"""insert into season_languages (season_id, name, module_id)
             select $to, f.name, f.module_id
               from (select name, module_id, min(id) as first from season_languages where season_id = $from
                      group by name, module_id) f
              where not exists (select 1 from season_languages t
                                 where t.season_id = $to and t.name = f.name and t.module_id = f.module_id)
              order by f.first""").transactionally).map(CopyLanguagesResult.Copied)
}
