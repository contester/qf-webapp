package controllers

import models.{SubmitError, SubmitErrors}
import play.api.data.Form
import play.api.mvc.{Result, Results}
import play.twirl.api.Html

/** What came of a submit attempt; the controller turns it into a response with `SubmitFlow.respond`. */
sealed trait SubmitOutcome
object SubmitOutcome {
  case class Accepted(id: Long) extends SubmitOutcome
  /** Refused with a message key (or text) to show as the form's global error. */
  case class Rejected(messageKey: String) extends SubmitOutcome
  /** The form did not validate; its own errors are shown. */
  case class Invalid(form: Form[SubmitData]) extends SubmitOutcome

  def of(res: Either[SubmitError, Long]): SubmitOutcome = res match {
    case Right(id) => Accepted(id)
    case Left(e) => Rejected(SubmitErrors.key(e))
  }
}

object SubmitFlow {
  /** Accepted goes to `accepted`; every refusal re-renders the form with a 400, never a 500. */
  def respond(parsed: Form[SubmitData], outcome: SubmitOutcome, render: Form[SubmitData] => Html, accepted: Result): Result =
    outcome match {
      case SubmitOutcome.Accepted(_) => accepted
      case SubmitOutcome.Rejected(key) => Results.BadRequest(render(parsed.withGlobalError(key)))
      case SubmitOutcome.Invalid(form) => Results.BadRequest(render(form))
    }
}
