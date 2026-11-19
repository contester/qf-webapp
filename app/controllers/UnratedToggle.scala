package controllers

import models.UnratedChoice
import org.stingray.contester.dbmodel.Unrated
import play.api.mvc.{Result, Results}

import scala.concurrent.{ExecutionContext, Future}

/** The unrated toggle of the admin participants page, without the controller wiring. */
object UnratedToggle {
  def labelKey(u: Option[Unrated]): String = s"admin.unrated.${UnratedChoice.value(u)}"

  /**
    * A value that is not `none`, `not_rated` or `disqualified` is a 400 and writes nothing; a participant without a
    * credential for the round (no row written) is a 404; otherwise `done`.
    */
  def respond(value: Option[String], write: Option[Unrated] => Future[Int], done: Option[Unrated] => Result)(
    implicit ec: ExecutionContext): Future[Result] =
    value.flatMap(UnratedChoice.parse) match {
      case None => Future.successful(Results.BadRequest("value must be none, not_rated or disqualified"))
      case Some(u) => write(u).map {
        case 0 => Results.NotFound("no such participant in this round")
        case _ => done(u)
      }
    }
}
