package pages

import models.ContestFixture
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import play.api.http.Writeable
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.{Application, ApplicationLoader, Environment, Mode, Play}

import scala.concurrent.{ExecutionContext, Future}

/**
  * The admin pages through the real application (MyApplicationLoader, its routes, filters and Silhouette) on a
  * fixture round of its own, logged in as the seed's admin (admin@example.org / secret). Skipped unless QF_TEST_URL
  * is set (see SubmitInsertDbSpec). The fixture exists before the application starts, so the round is known.
  */
class AdminPagesHttpSpec extends AnyFunSuite with BeforeAndAfterAll {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fx = new ContestFixture("Admin pages spec")

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import fx.one

  private var app: Application = _

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    if (fx.url.isDefined) {
      fx.create()
      val env = Environment.simple(mode = Mode.Test)
      val settings: Map[String, AnyRef] = Map(
        "play.application.loader" -> "MyApplicationLoader",
        "play.http.secret.key" -> "admin-pages-http-spec-secret-0123456789abcdef",
        "slick.dbs.default.profile" -> "org.stingray.contester.dbmodel.MyPostgresProfile$",
        "slick.dbs.default.db.driver" -> "org.postgresql.Driver",
        "slick.dbs.default.db.url" -> fx.url.get,
        "slick.dbs.default.db.user" -> fx.user,
        "slick.dbs.default.db.password" -> fx.password,
        "slick.dbs.default.db.numThreads" -> Integer.valueOf(4),
        "slick.dbs.default.db.maxConnections" -> Integer.valueOf(4),
        // nothing listens there: asset fetches fail fast and the pages render without them
        "fileserver.url" -> "http://127.0.0.1:9/fs/",
        "fileserver.shortn" -> "spec")
      val context = ApplicationLoader.Context.create(env, settings)
      app = ApplicationLoader(context).load(context) // play.application.loader: MyApplicationLoader
      Play.start(app)
    }
  }

  override protected def afterAll(): Unit = {
    try if (app != null) Play.stop(app)
    finally if (fx.url.isDefined) fx.cleanup()
    super.afterAll()
  }

  private def run[A](req: FakeRequest[A])(implicit w: Writeable[A]): Future[Result] = route(app, req).getOrElse(fail(s"no route for ${req.method} ${req.uri}"))

  private def form(method: String, uri: String, fields: (String, String)*) =
    FakeRequest(method, uri).withFormUrlEncodedBody(fields: _*).withHeaders("Csrf-Token" -> "nocheck")

  private lazy val adminSession: Seq[(String, String)] = {
    val r = run(form(POST, "/admin/login", "username" -> "admin@example.org", "password" -> "secret"))
    assert(status(r) == SEE_OTHER, contentAsString(r))
    session(r).data.toSeq
  }

  private def asAdmin[A](req: FakeRequest[A]): FakeRequest[A] = req.withSession(adminSession: _*)

  private def get(uri: String) = run(asAdmin(FakeRequest(GET, uri)))

  private def post(uri: String, fields: (String, String)*) = run(asAdmin(form(POST, uri, fields: _*)))

  private def unrated(participant: Int): Option[String] = {
    val r = fx.round
    one(sql"select unrated::text from round_credentials where round_id = $r and participant_id = $participant".as[Option[String]])
  }

  private def ok(uri: String): String = {
    val r = get(uri)
    assert(status(r) == OK, s"$uri: ${status(r)} ${contentAsString(r).take(500)}")
    contentAsString(r)
  }

  test("participants page lists the round's participants with their state") {
    assume(fx.url.isDefined)
    val page = ok(s"/admin/${fx.round}/participants")
    assert(page.contains(fx.kidName) && page.contains(s"participant-${fx.kid}"))
    assert(page.contains("""value="not_rated"""") && page.contains("""value="disqualified""""))
  }

  test("unrated toggle: each value is written; unknown value 400, unknown participant 404") {
    assume(fx.url.isDefined)
    val uri = s"/admin/${fx.round}/participants/${fx.kid}/unrated"
    for ((value, column) <- Seq("not_rated" -> Some("not_rated"), "disqualified" -> Some("disqualified"), "none" -> None)) {
      val r = post(uri, "value" -> value)
      assert(status(r) == SEE_OTHER && redirectLocation(r).contains(s"/admin/${fx.round}/participants"), value)
      assert(unrated(fx.kid) == column, value)
    }
    assert(status(post(uri, "value" -> "bogus")) == BAD_REQUEST)
    assert(status(post(uri)) == BAD_REQUEST)
    assert(unrated(fx.kid).isEmpty)
    assert(status(post(s"/admin/${fx.round}/participants/${Int.MaxValue}/unrated", "value" -> "not_rated")) == NOT_FOUND)
  }

  test("non-admins cannot see the participants or toggle: no session, or a contestant's session") {
    assume(fx.url.isDefined)
    val page = run(FakeRequest(GET, s"/admin/${fx.round}/participants"))
    assert(status(page) == SEE_OTHER && redirectLocation(page).contains("/admin/login"))
    val uri = s"/admin/${fx.round}/participants/${fx.kid}/unrated"
    val anon = run(form(POST, uri, "value" -> "disqualified"))
    assert(status(anon) == SEE_OTHER && redirectLocation(anon).contains("/admin/login"))

    val login = one(sql"select login from round_credentials where participant_id = ${fx.kid}".as[String])
    val team = run(form(POST, "/login", "username" -> login, "password" -> "777000"))
    assert(status(team) == SEE_OTHER && !redirectLocation(team).exists(_.contains("login")), "the contestant logs in")
    val teamSession = session(team).data.toSeq
    val asTeam = run(form(POST, uri, "value" -> "disqualified").withSession(teamSession: _*))
    assert(status(asTeam) == SEE_OTHER && redirectLocation(asTeam).contains("/admin/login"))
    assert(status(run(FakeRequest(GET, "/admin/languages").withSession(teamSession: _*))) == SEE_OTHER)
    assert(unrated(fx.kid).isEmpty)
  }

  test("languages page lists the current season's languages; copying from the current season is refused") {
    assume(fx.url.isDefined)
    val page = ok("/admin/languages")
    val current = one(sql"select id from seasons where is_current".as[Int])
    val names = fx.await(fx.db.run(sql"select name from season_languages where season_id = $current".as[String]))
    assert(names.nonEmpty && names.forall(n => page.contains(play.twirl.api.HtmlFormat.escape(n).body)))
    val before = names.size
    val r = post("/admin/languages/copy", "from" -> current.toString)
    assert(status(r) == SEE_OTHER && flash(r).get("error").exists(_.nonEmpty))
    assert(status(post("/admin/languages/copy", "from" -> "x")) == SEE_OTHER)
    assert(fx.await(fx.db.run(sql"select count(*) from season_languages where season_id = $current".as[Int].head)) == before)
  }

  test("clarifications: the Q&A page, a new clarification, and its edit form") {
    assume(fx.url.isDefined)
    ok(s"/admin/qanda/${fx.round}")
    ok(s"/admin/postNewClarification/${fx.round}")
    val r = post(s"/admin/postClarification/${fx.round}", "problem" -> "Z", "text" -> "Admin pages spec note", "isHidden" -> "false")
    assert(status(r) == SEE_OTHER && redirectLocation(r).contains(s"/admin/qanda/${fx.round}"))
    val rn = fx.round
    val id = one(sql"select id from clarifications where round_id = $rn and text = 'Admin pages spec note'".as[Long])
    assert(ok(s"/admin/qanda/${fx.round}").contains("Admin pages spec note"))
    ok(s"/admin/updateClarification/$id")
  }

  test("extra info: list, new, add") {
    assume(fx.url.isDefined)
    ok(s"/admin/${fx.round}/extrainfo")
    ok(s"/admin/${fx.round}/extrainfo/new")
    val r = post(s"/admin/${fx.round}/extrainfo", "num" -> "1", "heading" -> "Spec page", "data" -> "<p>x</p>")
    assert(status(r) == SEE_OTHER)
    assert(ok(s"/admin/${fx.round}/extrainfo").contains("Spec page"))
    ok(s"/admin/${fx.round}/extrainfo/1")
  }

  test("custom tests: list, show, rerun") {
    assume(fx.url.isDefined)
    val id = fx.insertCustomTest()
    assert(ok(s"/admin/customtests/${fx.round}").contains(s"/admin/customtest/${fx.round}/$id"))
    ok(s"/admin/customtest/${fx.round}/$id")
    val r = post(s"/admin/customtest/${fx.round}/$id/rerun")
    assert(status(r) == SEE_OTHER)
    assert(redirectLocation(r).exists(l => l.startsWith(s"/admin/customtest/${fx.round}/") && !l.endsWith(s"/$id")))
  }

  test("submits, a submit, the rejudge page and a problem rejudge") {
    assume(fx.url.isDefined)
    val id = fx.insertSubmit()
    fx.finishSubmit(id, passed = 2)
    assert(ok(s"/admin/submits/${fx.round}").contains(s"result-row-$id"))
    val shown = ok(s"/admin/showsubmit/${fx.round}/$id")
    assert(shown.contains("Задача Z") && shown.contains("(revision 1)"))
    assert(ok(s"/admin/rejudge/${fx.round}").contains("revision 1"))
    val r = post(s"/admin/rejudgeProblem/${fx.round}", "problem" -> "Z", "all" -> "true")
    assert(status(r) == SEE_OTHER && flash(r).get("success").exists(_.startsWith("1 ")))
    assert(one(sql"select testing_id is null from submits where id = $id".as[Boolean]))
  }

  test("the round list and the live monitor") {
    assume(fx.url.isDefined)
    assert(ok(s"/admin/${fx.round}/contests/").contains(fx.roundName))
    ok(s"/admin/monitor/${fx.round}")
  }

  test("CSRF: an admin POST carrying the session cookie but no token is refused; the AJAX bypass header passes") {
    assume(fx.url.isDefined)
    def bare(uri: String) = asAdmin(FakeRequest(POST, uri).withFormUrlEncodedBody("x" -> "1"))
    lazy val adminCookieHeader: String = {
      val r = run(form(POST, "/admin/login", "username" -> "admin@example.org", "password" -> "secret"))
      assert(status(r) == SEE_OTHER)
      new play.api.mvc.DefaultCookieHeaderEncoding().encodeCookieHeader(cookies(r).toSeq)
    }
    for (uri <- Seq("/admin/toggleClarification/999999999", s"/admin/deleteClarification/${fx.round}/999999999",
      "/admin/rejudgeSubmit/999999999")) {
      // the session travels in the Cookie header, as from a browser
      val req = FakeRequest(POST, uri).withFormUrlEncodedBody("x" -> "1").withHeaders("Cookie" -> adminCookieHeader)
      assert(status(run(req)) == FORBIDDEN, s"$uri without a token")
      assert(status(run(req.withHeaders("X-Requested-With" -> "XMLHttpRequest"))) != FORBIDDEN, s"$uri with the jQuery header")
    }
    // the clarification really is unknown: the request got past the filter
    assert(status(run(bare("/admin/toggleClarification/999999999").withHeaders("X-Requested-With" -> "XMLHttpRequest"))) == NOT_FOUND)
  }

  test("deleteClarification refuses a clarification of another round") {
    assume(fx.url.isDefined)
    val other = one(sql"select id from contest_rounds where id <> ${fx.round} and season_id = ${fx.season} order by id limit 1".as[Int])
    val clr = one(sql"insert into clarifications (season_id, round_id, problem, text) values (${fx.season}, $other, 'A', 'foreign') returning id".as[Long])
    try {
      val r = run(asAdmin(form(POST, s"/admin/deleteClarification/${fx.round}/$clr")))
      assert(status(r) == FORBIDDEN)
      assert(one(sql"select count(*) from clarifications where id = $clr".as[Int]) == 1)
      val own = one(sql"insert into clarifications (season_id, round_id, problem, text) values (${fx.season}, ${fx.round}, 'A', 'own') returning id".as[Long])
      assert(status(run(asAdmin(form(POST, s"/admin/deleteClarification/${fx.round}/$own")))) == OK)
    } finally fx.exec(sqlu"delete from clarifications where id = $clr")
  }
}
