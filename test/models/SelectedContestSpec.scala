package models

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest
import play.api.i18n.{DefaultLangs, DefaultMessagesApi, Lang}
import play.api.test.FakeRequest

class SelectedContestSpec extends AnyFunSuite {
  private val admin = Admin("a@example.org", "h")
  private def c(id: Int) = Contest(id, s"Round $id", DateTime.now, DateTime.now.plusHours(2), None, 1, false)

  test("no current rounds: nothing to select (the pages then render admin.noRounds, not a 500)") {
    assert(SelectedContest.choose(Nil, 5, admin).isEmpty)
  }

  test("the asked round, or the first one by id; the selector lists every round by id") {
    val all = Seq(c(7), c(3), c(5))
    val five = SelectedContest.choose(all, 5, admin).get
    assert(five.contest.id == 5 && five.contests.map(_._1) == Seq(3, 5, 7))
    assert(SelectedContest.choose(all, 42, admin).get.contest.id == 3)
  }

  test("the no-rounds page says «Нет текущих туров»") {
    val ru = Map("admin.noRounds" -> "Нет текущих туров", "admin.noRounds.hint" -> "hint", "admin.languages" -> "Языки")
    implicit val messages: play.api.i18n.Messages =
      new DefaultMessagesApi(Map("ru" -> ru), new DefaultLangs(Seq(Lang("ru")))).preferred(Seq(Lang("ru")))
    implicit val request: play.api.mvc.RequestHeader = FakeRequest()
    val page = views.html.admin.norounds().body
    assert(page.contains("Нет текущих туров") && page.contains("/admin/languages"))
    for (f <- Seq("conf/messages", "conf/messages.ru"))
      assert(new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(f)), "UTF-8").contains("\nadmin.noRounds="), f)
    assert(new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get("conf/messages.ru")), "UTF-8")
      .contains("admin.noRounds=Нет текущих туров"))
  }
}
