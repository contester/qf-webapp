package controllers

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, Team}
import palevo.query.{AudienceView, ContestSelection, Matching, TeamCounts, TeamNames, QueryConfig, QueryJson, QuerySource, Scope}
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
  /** SHA-256 of the token: both sides are compared as digests of the same length, so neither the content nor the
    * length of the configured token leaks through timing. */
  private def digest(s: String): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(s.getBytes(StandardCharsets.UTF_8))

  private val expected: Option[Array[Byte]] = cfg.token.map(digest)

  private val BearerPrefix = "Bearer "

  /** 404 when the API is disabled, 401 unless the request carries the configured bearer token. */
  private[controllers] def check(request: RequestHeader): Option[Result] =
    expected match {
      case None => Some(NotFound)
      case Some(want) =>
        val given = request.headers.get(AUTHORIZATION).filter(_.startsWith(BearerPrefix))
          .map(h => digest(h.substring(BearerPrefix.length)))
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

  /** GET /contest: times, team counts and, once started, the problem list. The list comes from the audience's
    * board; only for audience all does a missing board fall back to the stored problem names (the public monitor
    * may refuse to show a contest, and the public must then not learn its problems either). The team counts come
    * from the participant list, are the same for both audiences and are left out when their lookup fails. */
  def contest = authed { request =>
    withContests(request) { sel =>
      Future.traverse(sel.contests) { case (c, base) =>
        val countsF = optionalTeamCounts(c.id)
        val restF =
          if (!started(c, sel.now)) Future.successful(Json.obj())
          else {
            val boardF = source.board(c.id, real = isJury(sel))
            val namesF = source.problemNames(c.id)
            for {
              board <- boardF
              names <- namesF
            } yield board.map(_.problems) match {
              case Some(ps) => QueryJson.problems(ps, names)
              case None if isJury(sel) => QueryJson.problems(names.keys.toSeq.sorted, names)
              case None => Json.obj("note" -> "board unavailable")
            }
          }
        for {
          counts <- countsF
          rest <- restF
        } yield base ++ QueryJson.contestTimes(c, sel.now) ++ counts.fold(Json.obj())(QueryJson.teamCounts) ++ rest
      }
    }
  }

  /** Team counts are optional: a failed lookup leaves them out (logged) instead of failing the request. */
  private def optionalTeamCounts(contestId: Int): Future[Option[TeamCounts]] =
    Try(source.teamCounts(contestId)).fold(Future.failed, identity).map(Option(_)).recover { case NonFatal(e) =>
      logger.warn(s"palevo query teamCounts($contestId) failed, leaving out teams: ${e.getClass.getName}: ${e.getMessage}")
      None
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

  /** GET /team?q=…|id=…: candidates among the board's teams and, when exactly one, its row and cells. */
  def team = authed { request =>
    val q = request.getQueryString("q")
    val idParam = request.getQueryString("id").map(s => Try(s.trim.toInt).toOption)
    if (q.isEmpty && idParam.isEmpty) badRequest("q or id is required")
    else if (idParam.contains(None)) badRequest("id must be an integer")
    else {
      val wanted = idParam.flatten
      withContests(request) { sel =>
        Future.traverse(sel.contests) { case (c, base) =>
          if (!started(c, sel.now)) Future.successful(base ++ Json.obj("candidates" -> Json.arr()))
          else source.board(c.id, real = isJury(sel)).flatMap {
            case None => Future.successful(base ++ Json.obj("candidates" -> Json.arr(), "note" -> "board unavailable"))
            case Some(b) =>
              val found = wanted match {
                case Some(id) => b.rows.filter(_.team.id == id).take(1)
                case None => resolveExact(Matching.rankTeamsScored(q.getOrElse(""), b.rows)(r => teamNames(r.team)))
              }
              val cands = Json.obj("candidates" -> found.map(candidate))
              found match {
                case Seq(row) => teamDetail(c, sel, b.problems, row).map(t => base ++ cands ++ Json.obj("team" -> t))
                case _ => Future.successful(base ++ cands)
              }
          }
        }
      }
    }
  }

  private def teamNames(t: Team) = TeamNames(t.schoolShortName, t.schoolFullName, t.teamNum, t.teamName)

  /** A single top candidate with an exact match wins alone; otherwise all candidates stay (ambiguous when > 1). */
  private def resolveExact[A](scored: Seq[(A, Int)]): Seq[A] = scored.filter(_._2 == Matching.Exact) match {
    case Seq((only, _)) => Seq(only)
    case _ => scored.map(_._1)
  }

  private def candidate(r: live.LiveRow): JsObject =
    Json.obj("id" -> r.team.id, "name" -> r.team.teamFullName, "school_short" -> r.team.schoolShortName,
      "num" -> r.team.teamNum)

  private def cellJson(cell: live.LiveCell): JsObject = cell match {
    case live.LiveCell.Solved(a, secs) => Json.obj("state" -> "ok", "attempts" -> a, "minutes" -> secs / 60)
    case live.LiveCell.Failed(a) => Json.obj("state" -> "fail", "attempts" -> a)
    case live.LiveCell.Pending(f) => Json.obj("state" -> "pending", "attempts" -> f)
    case live.LiveCell.Empty => Json.obj("state" -> "empty", "attempts" -> 0)
  }

  private def teamDetail(c: Contest, sel: Selection, problemIds: Seq[String], row: live.LiveRow): Future[JsObject] = {
    val id = row.team.id
    val submitF = source.lastSubmits(c.id, Seq(id))
    // The room is a jury-only fact: for the public audience it is not even looked up.
    val roomF = if (isJury(sel)) source.teamRooms(c.id, Seq(id)) else Future.successful(Map.empty[Int, String])
    for {
      submits <- submitF
      rooms <- roomF
    } yield {
      val problems = JsObject(problemIds.map(p => p -> cellJson(row.cells.getOrElse(p, live.LiveCell.Empty))))
      val last = submits.get(id).map { t =>
        Json.obj("last_submit_at" -> QueryJson.iso(t),
          "seconds_since_last_submit" -> math.max(0L, (sel.now.getMillis - t.getMillis) / 1000))
      }.getOrElse(Json.obj())
      val room = rooms.get(id).map(r => Json.obj("room" -> r)).getOrElse(Json.obj())
      QueryJson.standingsRow(row) - "team" - "team_id" ++ Json.obj(
        "id" -> id, "name" -> row.team.teamFullName, "problems" -> problems) ++ last ++ room
    }
  }

  /** GET /problems?window=15m: per problem, board-derived counts plus recent arrivals and pending submits. */
  def problems = authed { request =>
    parseWindow(request.getQueryString("window")) match {
      case Left(e) => badRequest(e)
      case Right(minutes) =>
        withContests(request) { sel =>
          Future.traverse(sel.contests) { case (c, base) =>
            if (!started(c, sel.now)) Future.successful(base)
            else {
              val boardF = source.board(c.id, real = isJury(sel))
              val namesF = source.problemNames(c.id)
              val recentF = optionalCounts("recentArrivals")(source.recentArrivals(c.id, sel.now.minusMinutes(minutes)))
              val pendingF = optionalCounts("pendingCounts")(source.pendingCounts(c.id))
              for {
                board <- boardF
                names <- namesF
                recent <- recentF
                pending <- pendingF
              } yield {
                val withWindow = base ++ Json.obj("window_minutes" -> minutes)
                board match {
                  case None => withWindow ++ Json.obj("problems" -> Json.arr(), "note" -> "board unavailable")
                  case Some(b) =>
                    withWindow ++ Json.obj("problems" -> b.problems.map { p =>
                      val cells = b.rows.map(_.cells.getOrElse(p, live.LiveCell.Empty))
                      val pname: String = names.getOrElse(p, p)
                      val nrecent: Int = recent.getOrElse(p, 0)
                      val npending: Int = pending.getOrElse(p, 0)
                      Json.obj(
                        "id" -> p,
                        "name" -> pname,
                        "solved" -> cells.count(_.isInstanceOf[live.LiveCell.Solved]),
                        "teams_attempted" -> cells.count(_ != live.LiveCell.Empty),
                        "attempts" -> cells.map {
                          case live.LiveCell.Solved(a, _) => a
                          case live.LiveCell.Failed(a) => a
                          case live.LiveCell.Pending(f) => f
                          case live.LiveCell.Empty => 0
                        }.sum,
                        "recent_submits" -> nrecent,
                        "pending" -> npending)
                    })
                }
              }
            }
          }
        }
    }
  }

  /** Activity counts are optional: a failed lookup degrades to no counts (logged) instead of failing the request. */
  private def optionalCounts(what: String)(f: => Future[Map[String, Int]]): Future[Map[String, Int]] =
    Try(f).fold(Future.failed, identity).recover { case NonFatal(e) =>
      logger.warn(s"palevo query $what failed, answering zeros: ${e.getClass.getName}: ${e.getMessage}")
      Map.empty[String, Int]
    }

  private val MaxComputers = 10
  private val PrintJobLimit = 10

  /** GET /computers?q=…|area=…: both audiences, the same rows; global (no contest scope, `contests` optional but
    * validated). */
  def computers = authed { request =>
    val q = request.getQueryString("q")
    val area = request.getQueryString("area")
    Scope.parse(request.getQueryString("audience"), request.getQueryString("contests").orElse(Some("*"))) match {
      case Left(e) => badRequest(e)
      case Right(_) if q.isEmpty && area.isEmpty => badRequest("q or area is required")
      case Right(_) =>
        guarded(request.path) {
          source.computers().map { all =>
            val found = (q, area) match {
              case (Some(text), _) => all.filter(c => Matching.computerMatches(text, c.name))
              case (None, Some(a)) =>
                val want = Matching.normalize(a)
                all.filter(c => want.nonEmpty && Matching.normalize(c.area) == want)
              case _ => Nil
            }
            val shown = found.sortBy(_.name).take(MaxComputers)
            val ambiguous = shown.size > 1 && shown.map(_.area).distinct.size > 1
            Ok(Json.obj("as_of" -> QueryJson.iso(now()),
              "computers" -> shown.map(AudienceView.computer),
              "ambiguous" -> ambiguous))
          }
        }
    }
  }

  private val MaxAreas = 100

  /** GET /areas: both audiences, the same rows; global like `/computers`. Every area (room) with its number of
    * computers, sorted by name, and the total number of areas (`areas` is capped at 100). */
  def areas = authed { request =>
    Scope.parse(request.getQueryString("audience"), request.getQueryString("contests").orElse(Some("*"))) match {
      case Left(e) => badRequest(e)
      case Right(_) =>
        guarded(request.path) {
          source.computers().map { all =>
            val byArea = all.groupBy(_.area).toSeq.sortBy(_._1)
            Ok(Json.obj("as_of" -> QueryJson.iso(now()),
              "total" -> byArea.size,
              "areas" -> byArea.take(MaxAreas).map { case (a, cs) => Json.obj("name" -> a, "computers" -> cs.size) }))
          }
        }
    }
  }

  /** GET /printing?team=…&area=…: both audiences; recent print jobs of the selected contests and global printer
    * problems. The public gets no print agent names and no raw computer addresses (see `AudienceView`). */
  def printing = authed { request =>
    val teamParam = request.getQueryString("team").map(s => Try(s.trim.toInt).toOption)
    val area = request.getQueryString("area").map(_.trim).filter(_.nonEmpty)
    if (teamParam.contains(None)) badRequest("team must be an integer")
    else
      selecting(request) { sel =>
        val audience = sel.scope.audience
        Future.traverse(sel.contests) { case (c, base) =>
          source.printJobs(Seq(c.id), teamParam.flatten, area, PrintJobLimit).map { js =>
            base ++ Json.obj("print_jobs" -> js.map(AudienceView.printJob(_, audience)))
          }
        }.map { objs =>
          val printers = source.printerProblems().map(AudienceView.printer(_, audience))
          val body = Json.obj("as_of" -> QueryJson.iso(sel.now), "contests" -> objs, "printers" -> printers)
          Ok(if (objs.isEmpty) body + ("note" -> Json.toJson("no contest")) else body)
        }
      }
  }

}

object PalevoQuery {
  /** What `PalevoQuery#selecting` hands an endpoint: the request time, the parsed scope and the selected contests
    * with their base JSON objects. */
  case class Selection(now: DateTime, scope: Scope, contests: Seq[(Contest, JsObject)])

  val DefaultWindow = 15
  val MaxWindow = 120

  /** `window`: absent → 15; `<n>m` with n in 1..120; anything else is an error. */
  def parseWindow(s: Option[String]): Either[String, Int] = s match {
    case None => Right(DefaultWindow)
    case Some(v) =>
      val n = if (v.matches("[0-9]{1,4}m")) Some(v.dropRight(1).toInt).filter(x => x >= 1 && x <= MaxWindow) else None
      n.toRight("window must be <n>m with n from 1 to 120")
  }

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
