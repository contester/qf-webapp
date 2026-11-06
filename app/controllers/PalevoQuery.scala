package controllers

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import palevo.query.{QueryConfig, QuerySource}
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.Future

/** Read-only JSON query API for palevo's chat bot, under /api/palevo/v1. Bearer-token authenticated; no session or
 * cookie is read or created. With no token configured every endpoint answers 404. The token is never logged. */
class PalevoQuery(cc: ControllerComponents, cfg: QueryConfig, source: QuerySource) extends AbstractController(cc) {
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

  // Temporary: removed once the real endpoints exist.
  def ping = authed { _ => Future.successful(Ok(Json.obj("ok" -> true))) }
}
