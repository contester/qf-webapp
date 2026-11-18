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

  test("the controller maps errors to a 400 form, publishes nothing to RabbitMQ and still tells the live monitor") {
    val app = read("app/controllers/Application.scala")
    val at = app.indexOf("def submitPost")
    val body = app.substring(at, app.indexOf("def ackMessage"))
    assert(body.contains("Submits.insert("))
    assert(body.contains("SubmitErrors.withError"))
    assert(body.contains("BadRequest("))
    assert(body.contains("liveArrived("))
    assert(!body.contains("rabbitMq"))
    assert(!app.contains("rabbitMqModel"))
  }
}
