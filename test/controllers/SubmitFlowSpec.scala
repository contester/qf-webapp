package controllers

import models._
import org.scalatest.funsuite.AnyFunSuite
import play.api.i18n.{DefaultMessagesApi, Lang, Messages}
import play.api.data.Form

import java.nio.file.{Files, Paths}

/**
  * The submit flow without full controller wiring (it needs Silhouette, Slick and the live monitor): the mapping
  * of each SubmitError to a form error, the message keys in both languages, and the wiring of the controller.
  */
class SubmitFlowSpec extends AnyFunSuite {
  private def read(p: String) = new String(Files.readAllBytes(Paths.get(p)), "UTF-8")

  private val all: Seq[SubmitError] = Seq(RoundNotRunning, UnknownProblem, UnknownLanguage, NotAdmitted)

  test("each error has its own message key") {
    assert(all.map(SubmitErrors.key) == Seq("submit.error.roundNotRunning", "submit.error.unknownProblem",
      "submit.error.unknownLanguage", "submit.error.notAdmitted"))
  }

  test("each error renders as a global form error with a Russian text, not the key") {
    val ru = Map(
      "submit.error.roundNotRunning" -> "Соревнование сейчас не идёт",
      "submit.error.unknownProblem" -> "Нет такой задачи",
      "submit.error.unknownLanguage" -> "Нет такого языка",
      "submit.error.notAdmitted" -> "Вы не допущены к этому туру")
    val messages = new DefaultMessagesApi(Map("ru" -> ru), new play.api.i18n.DefaultLangs(Seq(Lang("ru")))).preferred(Seq(Lang("ru")))
    val form = Form(play.api.data.Forms.single("problem" -> play.api.data.Forms.text))
    for (e <- all) {
      val f = SubmitErrors.withError(form, e)
      assert(f.hasGlobalErrors)
      assert(messages(f.globalError.get.message) == ru(SubmitErrors.key(e)))
    }
  }

  test("both message files define every key; the Russian one has the specified texts") {
    for (f <- Seq("conf/messages", "conf/messages.ru"); e <- all)
      assert(read(f).contains(SubmitErrors.key(e) + "="), s"$f lacks ${SubmitErrors.key(e)}")
    val ru = read("conf/messages.ru")
    assert(ru.contains("submit.error.roundNotRunning=Соревнование сейчас не идёт"))
    assert(ru.contains("submit.error.notAdmitted=Вы не допущены к этому туру"))
  }

  private val ruMessages = {
    val ru = read("conf/messages.ru").linesIterator.filter(l => l.startsWith("submit.error.")).map { l =>
      val Array(k, v) = l.split("=", 2); k -> v
    }.toMap
    new DefaultMessagesApi(Map("ru" -> ru), new play.api.i18n.DefaultLangs(Seq(Lang("ru")))).preferred(Seq(Lang("ru")))
  }
  private val form = Form(play.api.data.Forms.mapping("problem" -> play.api.data.Forms.text,
    "compiler" -> play.api.data.Forms.number, "inline" -> play.api.data.Forms.text)(SubmitData.apply)(SubmitData.unapply))
  private val accepted = play.api.mvc.Results.SeeOther("/")

  test("every SubmitError becomes Rejected with its key; a Right becomes Accepted") {
    for (e <- all) assert(SubmitOutcome.of(Left(e)) == SubmitOutcome.Rejected(SubmitErrors.key(e)))
    assert(SubmitOutcome.of(Right(42L)) == SubmitOutcome.Accepted(42L))
  }

  test("a refusal re-renders the form with status 400 and the Russian text; nothing is a 500") {
    for (e <- all) {
      var shown: Option[Form[SubmitData]] = None
      val r = SubmitFlow.respond(form, SubmitOutcome.of(Left(e)), f => { shown = Some(f); play.twirl.api.Html("form") }, accepted)
      assert(r.header.status == 400)
      val f = shown.getOrElse(fail("form not rendered"))
      assert(f.globalError.map(_.message).contains(SubmitErrors.key(e)))
      assert(ruMessages(f.globalError.get.message) != SubmitErrors.key(e))
    }
  }

  test("an accepted submit redirects and renders nothing; an invalid form is a 400 with its own errors") {
    var rendered = false
    val ok = SubmitFlow.respond(form, SubmitOutcome.Accepted(1), _ => { rendered = true; play.twirl.api.Html("") }, accepted)
    assert(ok.header.status == 303 && ok.header.headers("Location") == "/" && !rendered)
    val bad = form.bind(Map("problem" -> "A"))
    val r = SubmitFlow.respond(form, SubmitOutcome.Invalid(bad), f => { assert(f.hasErrors); play.twirl.api.Html("") }, accepted)
    assert(r.header.status == 400)
  }

  // Not tested: the full Application.submitPost path through Silhouette (FakeRequest with an authenticated team). It
  // needs Silhouette, Slick and live-monitor fixtures well beyond a few lines; a tracked gap. Its logic is
  // Submits.insert (DB specs) plus SubmitOutcome/SubmitFlow (above).
}
