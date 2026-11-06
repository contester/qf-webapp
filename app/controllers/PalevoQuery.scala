package controllers

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest
import palevo.query.{ContestSelection, QueryConfig, QueryJson, QuerySource, Scope}
import play.api.Logging
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

/** Read-only JSON query API for palevo's chat bot, under /api/palevo/v1. Bearer-token authenticated; no session or
 * cookie is read or created. With no token configured every endpoint answers 404. The token is never logged. */
class PalevoQuery(cc: ControllerComponents, cfg: QueryConfig, source: QuerySource) extends AbstractController(cc)
  with Logging {
  import PalevoQuery._

  private implicit val ec: ExecutionContext = cc.executionContext

  /** The clock; specs override it. */
  protected def now(): DateTime = DateTime.now
  private val expected: Option[Array[Byte]] = cfg.token.map(_.getBytes(StandardCharsets.UTF_8))

  private val BearerPrefix = "Bearer "

  /** 404 when the API is disabled, 401 unless the request carries the configured bearer token. */
  private[controllers] def check(request: RequestHeader): Option[Result] =
    expected match {
      case None => Some(NotFound)
      case Some(want) =>
        val given = request.headers.get(AUTHORIZATION).filter(_.startsWith(BearerPrefix))
          .map(_.substring(BearerPrefix.length).getBytes(StandardCharsets.UTF_8))
        if (given.exists(MessageDigest.isEqual(_, want))) None
        else Some(Unauthorized(Json.obj("error" -> "unauthorized")))
    }

  private def authed(block: Request[AnyContent] => Future[Result]): Action[AnyContent] = Action.async { request =>
    check(request) match {
      case Some(denied) => Future.successful(denied)
      case None => block(request)
    }
  }

  private def badRequest(text: String): Future[Result] = Future.successful(BadRequest(QueryJson.error(text)))

  /** Runs `f`, turning a thrown or failed future into a 500 that carries no detail (the detail is logged). */
  private def guarded(what: String)(f: => Future[Result]): Future[Result] =
    Try(f).fold(Future.failed, identity).recover { case NonFatal(e) =>
      logger.error(s"palevo query $what failed", e)
      InternalServerError(QueryJson.error("internal error"))
    }

  /**
    * Scope parsing and contest selection shared by every contest-bound endpoint. Parses `audience`/`contests`
    * and the optional `contest` (400 with `{"error":…}` when invalid or not selectable), loads and selects the
    * contests and hands `f` the selection, in selection order, each contest with its base object
    * (`id`, `name`, `status`, `view`). Unlike [[withContests]] it also calls `f` when nothing is selected, and `f`
    * builds the whole response. Any failure is a 500 `{"error":"internal error"}`.
    */
  private[controllers] def selecting(request: RequestHeader)(f: Selection => Future[Result]): Future[Result] =
    Scope.parse(request.getQueryString("audience"), request.getQueryString("contests")) match {
      case Left(e) => badRequest(e)
      case Right(scope) =>
        request.getQueryString("contest").map(s => Try(s.toInt).toOption) match {
          case Some(None) => badRequest("contest must be an id")
          case explicit =>
            guarded(request.path) {
              val at = now()
              source.contests().flatMap { all =>
                ContestSelection.select(all, scope, cfg.allowedContests, at, explicit.flatten) match {
                  case Left(e) => badRequest(e)
                  case Right(cs) =>
                    f(Selection(at, scope, cs.map(c => c -> QueryJson.contestBase(c, scope.audience, at))))
                }
              }
            }
        }
    }

  /**
    * [[selecting]] for endpoints whose answer is the plain envelope: with no selected contest it answers the
    * `no contest` envelope without calling `f`; otherwise `f` returns one object per selected contest (normally
    * the base object plus endpoint fields) and the answer is `{"as_of", "contests": [...]}`.
    */
  private[controllers] def withContests(request: RequestHeader)(f: Selection => Future[Seq[JsObject]]): Future[Result] =
    selecting(request) { sel =>
      if (sel.contests.isEmpty) Future.successful(Ok(QueryJson.noContest(sel.now)))
      else f(sel).map(objs => Ok(QueryJson.envelope(sel.now, objs)))
    }

  private def started(c: Contest, at: DateTime) = ContestSelection.status(c, at) != "before"

  private def isJury(sel: Selection) = sel.scope.audience == palevo.query.Jury

  /** GET /contest: times and, once started, the problem list. */
  def contest = authed { request =>
    withContests(request) { sel =>
      Future.traverse(sel.contests) { case (c, base) =>
        val withTimes = base ++ QueryJson.contestTimes(c, sel.now)
        if (!started(c, sel.now)) Future.successful(withTimes)
        else {
          val boardF = source.board(c.id, real = isJury(sel))
          val namesF = source.problemNames(c.id)
          for {
            board <- boardF
            names <- namesF
          } yield withTimes ++ QueryJson.problems(board.map(_.problems).getOrElse(names.keys.toSeq.sorted), names)
        }
      }
    }
  }

  /** GET /standings?top=N: the top rows of the audience's board (freezing for public, real for all). */
  def standings = authed { request =>
    parseTop(request.getQueryString("top")) match {
      case Left(e) => badRequest(e)
      case Right(top) =>
        withContests(request) { sel =>
          Future.traverse(sel.contests) { case (c, base) =>
            if (!started(c, sel.now)) Future.successful(base)
            else source.board(c.id, real = isJury(sel)).map {
              case None => base ++ Json.obj("rows" -> Json.arr(), "note" -> "board unavailable")
              case Some(b) => base ++ Json.obj("teams" -> b.rows.size, "rows" -> b.rows.take(top).map(QueryJson.standingsRow))
            }
          }
        }
    }
  }

  // Temporary: removed once the real endpoints exist.
  def ping = authed { _ => Future.successful(Ok(Json.obj("ok" -> true))) }
}

object PalevoQuery {
  /** What [[PalevoQuery.selecting]] hands an endpoint: the request time, the parsed scope and the selected contests
    * with their base JSON objects. */
  case class Selection(now: DateTime, scope: Scope, contests: Seq[(Contest, JsObject)])

  val DefaultTop = 10
  val MaxTop = 30

  /** `top`: absent → 10; a positive integer, capped at 30; anything else is an error. */
  def parseTop(s: Option[String]): Either[String, Int] = s match {
    case None => Right(DefaultTop)
    case Some(v) => Try(v.toInt).toOption.filter(_ > 0) match {
      case Some(n) => Right(math.min(n, MaxTop))
      case None => Left("top must be a positive integer")
    }
  }
}
