package models

import org.postgresql.util.PSQLException
import org.stingray.contester.dbmodel.{Extrainfo, SlickModel}
import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}

/** Admin-side editing of the extra pages ("extra_info" rows) that teams see in their navbar. */
object AdminExtraInfo {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  sealed trait SaveResult
  case object Saved extends SaveResult
  /** Another page of the contest already has the requested number (primary key (contest, num)). */
  case object NumberTaken extends SaveResult
  /** The page being edited is gone (deleted meanwhile). */
  case object Missing extends SaveResult

  private val UniqueViolation = "23505"

  /** All pages of a contest, in the order teams see them. */
  def list(db: JdbcBackend#DatabaseDef, contest: Int): Future[Seq[Extrainfo]] =
    db.run(SlickModel.extraInfos.filter(_.contest === contest).sortBy(_.num).result)

  def get(db: JdbcBackend#DatabaseDef, contest: Int, num: Int)(implicit ec: ExecutionContext): Future[Option[Extrainfo]] =
    db.run(SlickModel.extraInfos.filter(x => x.contest === contest && x.num === num).result.headOption)

  /**
   * Adds a page (`original` is None), or updates the page that has number `original`, possibly giving it a new
   * number. Unique numbers are the database's job; a clash comes back as [[NumberTaken]].
   */
  def save(db: JdbcBackend#DatabaseDef, contest: Int, original: Option[Int], page: Extrainfo)
          (implicit ec: ExecutionContext): Future[SaveResult] = {
    val action: DBIO[SaveResult] = original match {
      case None =>
        (SlickModel.extraInfos += page.copy(contest = contest)).map(_ => Saved: SaveResult)
      case Some(num) =>
        SlickModel.extraInfos.filter(x => x.contest === contest && x.num === num)
          .map(x => (x.num, x.heading, x.data)).update((page.num, page.heading, page.data))
          .map(n => if (n == 0) Missing: SaveResult else Saved: SaveResult)
    }
    db.run(action).recover {
      case e: PSQLException if e.getSQLState == UniqueViolation => NumberTaken
    }
  }

  /** True if there was such a page. */
  def delete(db: JdbcBackend#DatabaseDef, contest: Int, num: Int)(implicit ec: ExecutionContext): Future[Boolean] =
    db.run(SlickModel.extraInfos.filter(x => x.contest === contest && x.num === num).delete).map(_ > 0)
}
