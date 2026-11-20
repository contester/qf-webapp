package views

import controllers.serversideeval.ServerSideData
import controllers.{AuthData, ClarificationReqData, ClarificationResponse, PostClarification, SubmitData}
import models._
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel._
import play.api.data.Form
import play.api.data.Forms._
import play.api.http.HttpConfiguration
import play.api.i18n._
import play.api.mvc.request.{DefaultRequestFactory, RemoteConnection, RequestTarget}
import play.api.mvc.{Flash, Headers, RequestHeader}
import play.api.test.FakeRequest
import play.filters.csrf.CSRF

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._
import scala.io.Source

class I18nSpec extends AnyFunSuite {
  private def parseLines(path: String): Vector[(String, String)] = {
    val s = Source.fromFile(path, "UTF-8")
    try s.getLines().map(_.trim).filter(l => l.contains("=") && !l.startsWith("#")).map { l =>
      val i = l.indexOf('=')
      l.substring(0, i).trim -> l.substring(i + 1).trim
    }.toVector
    finally s.close()
  }

  private def loadMap(path: String): Map[String, String] = parseLines(path).toMap

  private val enMap = loadMap("conf/messages")
  private val ruMap = loadMap("conf/messages.ru")

  private val ruApi = new DefaultMessagesApi(Map("ru" -> ruMap, "default" -> enMap), new DefaultLangs(Seq(Lang("ru"))))
  private implicit val ruMessages: MessagesImpl = MessagesImpl(Lang("ru"), ruApi)

  private implicit val flash: Flash = Flash()
  private implicit val request: RequestHeader = new DefaultRequestFactory(HttpConfiguration()).createRequestHeader(
    RemoteConnection(java.net.InetAddress.getLoopbackAddress, secure = false, None), "GET",
    RequestTarget("/", "/", Map.empty), "HTTP/1.1", new Headers(Seq.empty), play.api.libs.typedmap.TypedMap.empty)
    .addAttr(CSRF.Token.InfoAttr, CSRF.TokenInfo(CSRF.Token("csrfToken", "test-token")))

  test("conf/messages and conf/messages.ru have identical key sets and no duplicates") {
    for (file <- Seq("conf/messages", "conf/messages.ru")) {
      val keys = parseLines(file).map(_._1)
      val dups = keys.groupBy(identity).collect { case (k, vs) if vs.size > 1 => k }
      assert(dups.isEmpty, s"$file has duplicate keys: $dups")
    }
    assert(enMap.keySet -- ruMap.keySet == Set.empty, "keys missing from conf/messages.ru")
    assert(ruMap.keySet -- enMap.keySet == Set.empty, "keys missing from conf/messages")
  }

  test("message entries contain no lone unescaped single quotes that MessageFormat would strip") {
    for ((file, entries) <- Seq("conf/messages" -> parseLines("conf/messages"), "conf/messages.ru" -> parseLines("conf/messages.ru"))) {
      for ((k, v) <- entries) {
        assert(!v.replace("''", "").contains("'"), s"$file key '$k' has an unescaped single quote: $v")
      }
    }
  }

  test("every literal messages(...) key in app/views and app/controllers exists in both message files") {
    val keyPattern = """(?:messages|request2Messages\(request\))\("([A-Za-z0-9_.]+)"[,)]""".r
    val files = Files.walk(Paths.get("app/views")).iterator().asScala.toSeq ++
      Files.walk(Paths.get("app/controllers")).iterator().asScala.toSeq
    val keys = files.filter(p => Files.isRegularFile(p) && (p.toString.endsWith(".scala") || p.toString.endsWith(".html"))).flatMap { p =>
      val content = new String(Files.readAllBytes(p), UTF_8)
      keyPattern.findAllMatchIn(content).map(m => p.toString -> m.group(1))
    }
    assert(keys.nonEmpty)
    for ((file, key) <- keys) {
      assert(enMap.contains(key), s"$file references missing key '$key' in conf/messages")
      assert(ruMap.contains(key), s"$file references missing key '$key' in conf/messages.ru")
    }
  }

  private def resolveLangs(configured: Langs): Langs = {
    val cls = Class.forName("I18nDefaults$")
    val inst = cls.getField("MODULE$").get(null)
    cls.getMethod("resolveLangs", classOf[Langs]).invoke(inst, configured).asInstanceOf[Langs]
  }

  test("I18nDefaults.resolveLangs defaults to Russian when unconfigured and respects explicit configuration") {
    assert(resolveLangs(new DefaultLangs(Nil)).availables == Seq(Lang("ru")))
    val explicit = new DefaultLangs(Seq(Lang("en")))
    assert(resolveLangs(explicit).availables == Seq(Lang("en")))
  }

  test("with default Russian langs, Accept-Language: en-US still resolves to Russian") {
    val langs = resolveLangs(new DefaultLangs(Nil))
    val api = new DefaultMessagesApi(Map("ru" -> ruMap, "default" -> enMap), langs)
    val req = FakeRequest().withHeaders("Accept-Language" -> "en-US,en;q=0.9")
    val resolved = api.preferred(req)
    assert(resolved.lang == Lang("ru"))
    assert(resolved("label.logout") == "Выход")
    assert(resolved("admin.languages") == "Языки")
  }

  private val now = new DateTime(2026, 11, 20, 12, 0)
  private val contest = Contest(3, "Round 3", now.minusHours(1), now.plusHours(2), Some("62042"), 1)
  private val selected = SelectedContest(contest, Seq(3 -> "Round 3"), canModify = true, isSuperAdmin = true)
  private val team = Team(1, 7, "Иван Иванов", "Школа 1", "Город", None)
  private val loggedIn = LoggedInTeam("u1", contest, team, Nil)
  private val admin = Admin("admin@example.org", "hash")

  test("admin languages page with a selected contest renders topbar.css, full admin navbar, and Russian UI") {
    val page = views.html.admin.languages(
      Some(1 -> "2026"),
      Seq(SeasonLanguage(1, "C++20", "g++")),
      Seq(SeasonWithLanguages(2, "2025", 3)),
      Some(selected)).body
    assert(page.contains("css/topbar.css"))
    assert(page.contains("Участники") && page.contains("Перепроверка") && page.contains("Туры"))
    assert(page.contains("3 - Round 3") && page.contains("listenOnAdmin"))
    assert(page.contains("Языки сезона 2026") && page.contains("Скопировать"))
    assert(page.contains("Выход") && !page.contains("Logout"))
  }

  test("admin languages page without a contest and norounds page both include topbar.css in Russian") {
    val langsPage = views.html.admin.languages(None, Nil, Nil, None).body
    assert(langsPage.contains("css/topbar.css") && langsPage.contains("Нет текущего сезона."))
    val noRounds = views.html.admin.norounds().body
    assert(noRounds.contains("css/topbar.css") && noRounds.contains("Нет текущих туров"))
    assert(noRounds.contains("Выход") && !noRounds.contains("Logout"))
  }

  test("all contestant and admin views render in Russian without leaking English UI strings") {
    val submitForm = Form(mapping("problem" -> text, "compiler" -> number, "inline" -> text)(SubmitData.apply)(SubmitData.unapply))
    val evalForm = Form(mapping("compiler" -> number, "inlinesolution" -> text, "inlinedata" -> text)(ServerSideData.apply)(ServerSideData.unapply))
    val clrReqForm = Form(mapping("problem" -> text, "text" -> text)(ClarificationReqData.apply)(ClarificationReqData.unapply))
    val authForm = Form(mapping("username" -> text, "password" -> text)(AuthData.apply)(AuthData.unapply))
    val postClrForm = Form(mapping("problem" -> text, "text" -> text, "isHidden" -> boolean)(PostClarification.apply)(PostClarification.unapply))
    val answerForm = Form(mapping("answer" -> text)(ClarificationResponse.apply)(ClarificationResponse.unapply))
    val extraForm = Form(mapping("num" -> number, "heading" -> text, "data" -> text)((n, h, d) => Extrainfo(3, n, h, d))(x => Some((x.num, x.heading, x.data))))

    val sub = Submit(SubmitId(10, Arrived(now, 120), 1, 3, RatedProblem("A", 10), "cpp"),
      finished = true, compiled = true, passed = 1, taken = 2, Some(100), Some(5), 10)
    val fs = FullyDescribedSubmit(sub, 1, SubmitWaiting, SubmitStats(TimeMs(10), Memory(2048)),
      Seq(ResultEntry(100, 0, 2, TimeMs(0), Memory(0), 0, 1)))
    val adminRow = AdminSubmitRow(sub, SubmitWaiting, SubmitStats(TimeMs(10), Memory(2048)), runs = 2)
    val evalEntry = EvalEntry(5, 3, 1, "C++", "code".getBytes(UTF_8), "in".getBytes(UTF_8), "out".getBytes(UTF_8),
      now, None, 10, TimeMs(10), Memory(1024), 0)
    val clr = Clarification(Some(1L), 3, "A", "Уточнение", now, hidden = false)
    val clrReq = ClarificationRequest(2L, 3, 1, "A", "Вопрос?", "", now, answered = true)
    val subDetails = SubmitDetails(fs, "int main(){}".getBytes(UTF_8))
    val runs = Seq(RunSummary(100, now, Some(now), "revision 1", ruMessages("run.accepted"), success = true, SubmitStats(TimeMs(10), Memory(1024)), current = true))
    val testRows = TestRows.describe(Seq(ResultRow(1, 13, TimeMs(10), Memory(1024), None, 0)), Nil, ruMessages)
    val board = MonitorPoints(Seq(ProblemColumn("A", "Alpha", 10)),
      Seq(RankedRow(Some(1), team, Seq(PointsCell(Some(5), 1, full = false)), 5)))

    val pages: Seq[(String, String)] = Seq(
      "sendsolution" -> views.html.sendsolution(loggedIn, submitForm, Problems.forForm(Seq(Problem(3, "A", "Alpha", 1, 10))), Compilers.forForm(Seq(Compiler(1, "C++", "g++")))).body,
      "sendwithinput" -> views.html.sendwithinput(loggedIn, evalForm, Compilers.forForm(Seq(Compiler(1, "C++", "g++"))), Seq(evalEntry)).body,
      "clarifications" -> views.html.clarifications(loggedIn, Seq(clr), Seq(clrReq), Seq("" -> ruMessages("label.selectProblem")), clrReqForm).body,
      "index" -> views.html.index(loggedIn, Seq(fs)).body,
      "login" -> views.html.login(authForm).body,
      "showsubmit" -> views.html.showsubmit(loggedIn, Some(subDetails), testRows, None).body,
      "loggedinmonitor" -> views.html.loggedinmonitor(contest, board, loggedIn).body,
      "admin/submits" -> views.html.admin.submits(AdminSubmitPage(Seq(adminRow), None, None), Map.empty, selected, admin, None, live = true, newest = true).body,
      "admin/customtests" -> views.html.admin.customtests(AdminCustomPage(Seq(AdminCustomRow(5, 1, "C++", now, None, 10, TimeMs(1), Memory(1024), 0, 4, 2, 0)), None, None), Map.empty, selected, None).body,
      "admin/showcustomtest" -> views.html.admin.showcustomtest(evalEntry, None, selected, canRerun = true).body,
      "admin/rejudge" -> views.html.admin.rejudge(selected, Seq(ProblemRevisionStatus("A", Some("revision 1"), 1, 1, 0, Seq("revision 0" -> 1), 2))).body,
      "admin/contestlist" -> views.html.admin.contestlist(Seq(contest), selected).body,
      "admin/extrainfo" -> views.html.admin.extrainfo(Seq(Extrainfo(3, 1, "Правила", "<p>ok</p>")), selected).body,
      "admin/editextrainfo" -> views.html.admin.editextrainfo(extraForm, Some(1), selected).body,
      "admin/qanda" -> views.html.admin.qanda(Seq(clr), Seq(clrReq), selected).body,
      "admin/postclarification" -> views.html.admin.postclarification(Some(1L), postClrForm, Seq("" -> ruMessages("label.selectProblem")), selected).body,
      "admin/postanswer" -> views.html.admin.postanswer(answerForm, clrReq, Nil, selected).body,
      "admin/contester" -> views.html.admin.contester(selected).body,
      "admin/participants" -> views.html.admin.participants(Seq(RoundParticipant(1, 7, "Иван", "Школа", "Город", "u1", isActive = true, None)), selected).body,
      "admin/languages" -> views.html.admin.languages(Some(1 -> "2026"), Seq(SeasonLanguage(1, "C++", "g++")), Nil, Some(selected)).body,
      "admin/showsubmit" -> views.html.admin.showsubmit(subDetails, selected, Map.empty, runs, Some(100L), testRows).body,
      "admin/login" -> views.html.admin.login(authForm).body
    )

    val forbidden = Seq(
      "Logout", "Browse...", "Custom tests", "Extra pages", "Open connections",
      "Rounds of the current season", "Clarification requests", "Rejudge", "Download",
      "No comments", "Select problem", "Select compiler", "Compilation error", "Running..."
    )

    for ((name, html) <- pages) {
      assert(html.contains("""<html lang="ru">"""), s"$name missing lang=ru")
      for (bad <- forbidden) {
        assert(!html.contains(bad), s"$name leaked English UI string '$bad'")
      }
    }
  }
}
