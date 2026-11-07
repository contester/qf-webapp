package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.ConfigFactory
import live.{LiveBoard, LiveCell}
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, LocalTeam, Team}
import palevo.query.{FakeSource, QueryConfig, QuerySource, TeamCounts}
import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.i18n.{DefaultLangs, DefaultMessagesApi}
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.typedmap.TypedMap
import play.api.mvc._
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class PalevoQueryContestSpec extends AnyFunSuite with BeforeAndAfterAll {
  private val system = ActorSystem("PalevoQueryContestSpec", ConfigFactory.parseString(
    """
      |akka.loglevel = WARNING
      |akka.stdout-loglevel = OFF
      |akka.use-slf4j = off
      |""".stripMargin))
  override def afterAll(): Unit = Await.result(system.terminate(), 5.seconds)

  private implicit val mat: Materializer = Materializer(system)
  private implicit val ec: ExecutionContext = system.dispatcher

  private val cc: ControllerComponents = {
    val parsers = PlayBodyParsers()
    DefaultControllerComponents(DefaultActionBuilder(parsers.defaultBodyParser), parsers, new DefaultMessagesApi(),
      new DefaultLangs(), new DefaultFileMimeTypes(FileMimeTypesConfiguration()), ec)
  }

  private val token = "s3cret-token"
  private val fixedNow = new DateTime(2026, 11, 6, 12, 0, 0, DateTimeZone.UTC)
  private def h(n: Int) = fixedNow.plusHours(n)

  private def contest(id: Int, start: Int, freeze: Int, end: Int, expose: Int) =
    Contest(id, s"Contest $id", h(start), h(freeze), h(end), h(expose), None, "en", schoolMode = false)

  private def team(id: Int): Team =
    LocalTeam(id, 1, s"School $id", s"S$id", 1, s"Team$id: Ivanov, Petrov", notRated = false, noPrint = false,
      disabled = false)

  import LiveCell._

  private val problemIds = Seq("A", "B", "C")
  private val names = Map("A" -> "Apples", "B" -> "Bananas", "C" -> "Cherries")

  // Freezing board: team 1 shows one solve, team 2 a pending cell. Real board: team 2 actually solved two.
  private val frozenBoard = LiveBoard.build(problemIds, Seq(team(1), team(2)),
    Map(1 -> Map("A" -> Solved(1, 600)), 2 -> Map("A" -> Pending(1))))
  private val realBoard = LiveBoard.build(problemIds, Seq(team(1), team(2)),
    Map(1 -> Map("A" -> Solved(1, 600)), 2 -> Map("A" -> Solved(1, 300), "B" -> Solved(2, 4000))))
  private val bigBoard = LiveBoard.build(problemIds, (1 to 35).map(team),
    (1 to 35).map(i => i -> Map("A" -> (Solved(1, i * 60): LiveCell))).toMap)

  // Running with the freeze in effect.
  private val frozenRunning = contest(1, -2, -1, 1, 2)
  // Running, no freeze configured (freeze == end).
  private val plainRunning = contest(2, -2, 1, 1, 1)
  // Upcoming.
  private val upcoming = contest(3, 1, 4, 5, 6)

  private class Ctl(source: QuerySource, allowed: Set[Int] = (1 to 10).toSet, tok: Option[String] = Some(token))
    extends PalevoQuery(cc, QueryConfig(tok, allowed), source) {
    override protected def now(): DateTime = fixedNow
    // The 500 tests fail on purpose; keep their stack traces out of the test output.
    override protected val logger: play.api.Logger = new play.api.Logger(org.slf4j.helpers.NOPLogger.NOP_LOGGER)
  }

  private def req(path: String, params: (String, String)*): Request[AnyContent] = {
    val q = params.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }
    val uri = path + (if (params.isEmpty) "" else params.map { case (k, v) => s"$k=$v" }.mkString("?", "&", ""))
    RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget(uri, path, q), "HTTP/1.1", Headers("Authorization" -> s"Bearer $token"), TypedMap.empty,
      AnyContentAsEmpty)
  }

  private def await[T](f: Future[T]): T = Await.result(f, 5.seconds)

  private case class Resp(status: Int, body: String) {
    lazy val json: JsValue = Json.parse(body)
    def contests: Seq[JsObject] = (json \ "contests").as[Seq[JsObject]]
    def only: JsObject = { assert(contests.size == 1, body); contests.head }
  }

  private def run(action: Action[AnyContent], r: Request[AnyContent]): Resp = {
    val res = await(action(r))
    Resp(res.header.status, await(res.body.consumeData).utf8String)
  }

  private def contestReq(c: Ctl, params: (String, String)*) = run(c.contest, req("/api/palevo/v1/contest", params: _*))
  private def standingsReq(c: Ctl, params: (String, String)*) =
    run(c.standings, req("/api/palevo/v1/standings", params: _*))

  private val pub = Seq("audience" -> "public", "contests" -> "*")
  private val jury = Seq("audience" -> "all", "contests" -> "*")

  private def fake(contests: Seq[Contest], frozen: Map[Int, LiveBoard] = Map.empty, real: Map[Int, LiveBoard] = Map.empty,
                   problems: Map[Int, Map[String, String]] = Map.empty, counts: Map[Int, TeamCounts] = Map.empty) =
    new FakeSource(contestList = contests, freezingBoards = frozen, realBoards = real, problems = problems,
      counts = counts)

  private def frozenFake() = fake(Seq(frozenRunning), Map(1 -> frozenBoard), Map(1 -> realBoard), Map(1 -> names),
    Map(1 -> TeamCounts(2, 1)))

  private def teamsObj(regular: Int, out: Int) =
    Json.obj("regular" -> regular, "out_of_competition" -> out, "total" -> (regular + out))

  // Between contests: 3 is over (results exposed), 4 starts tomorrow.
  private val finished3 = contest(3, -6, -3, -2, -1)
  private val next4 = contest(4, 20, 23, 25, 26)
  private def betweenFake() = fake(Seq(frozenRunning.copy(id = 1, startTime = h(-30), freezeTime = h(-29),
      endTime = h(-28), exposeTime = h(-27)), finished3, next4),
    Map(3 -> frozenBoard, 4 -> frozenBoard), Map(3 -> realBoard, 4 -> realBoard), Map(3 -> names, 4 -> names),
    Map(1 -> TeamCounts(9, 9), 3 -> TeamCounts(2, 1), 4 -> TeamCounts(40, 3)))

  private def rowTuples(c: JsObject) =
    (c \ "rows").as[Seq[JsObject]].map(r => ((r \ "rank").as[String], (r \ "team").as[String], (r \ "solved").as[Int]))

  test("standings, public, freeze in effect: the freezing board and view frozen") {
    val src = frozenFake()
    val r = standingsReq(new Ctl(src), pub: _*)
    assert(r.status == 200, r.body)
    val c = r.only
    assert((c \ "id").as[Int] == 1)
    assert((c \ "name").as[String] == "Contest 1")
    assert((c \ "status").as[String] == "running")
    assert((c \ "view").as[String] == "frozen")
    assert((c \ "teams").as[Int] == 2)
    assert(rowTuples(c) == Seq(
      ("1", team(1).teamFullName, 1),
      ("2", team(2).teamFullName, 0)))
    assert(src.calls.contains("board(1,false)"))
    assert(!src.calls.contains("board(1,true)"))
  }

  test("standings, all, freeze in effect: the real board and view live-jury") {
    val src = frozenFake()
    val r = standingsReq(new Ctl(src), jury: _*)
    assert(r.status == 200, r.body)
    val c = r.only
    assert((c \ "view").as[String] == "live-jury")
    assert(rowTuples(c) == Seq(
      ("1", team(2).teamFullName, 2),
      ("2", team(1).teamFullName, 1)))
    val first = (c \ "rows").as[Seq[JsObject]].head
    assert((first \ "penalty").as[Int] == realBoard.rowOf(2).get.score.penalty)
    assert(src.calls.contains("board(1,true)"))
    assert(!src.calls.contains("board(1,false)"))
  }

  test("rank 0 (not rated) is shown as *") {
    val nr = LocalTeam(9, 1, "Guests", "G", 0, "", notRated = true, noPrint = false, disabled = false)
    val b = LiveBoard.build(problemIds, Seq(team(1), nr), Map(9 -> Map("A" -> Solved(1, 60))))
    val r = standingsReq(new Ctl(fake(Seq(plainRunning), Map(2 -> b))), pub: _*)
    assert(rowTuples(r.only).map(_._1) == Seq("*", "1"))
  }

  test("no freeze configured: view no-freeze for both audiences") {
    val src = fake(Seq(plainRunning), Map(2 -> frozenBoard), Map(2 -> realBoard), Map(2 -> names))
    for (p <- Seq(pub, jury)) {
      assert((standingsReq(new Ctl(src), p: _*).only \ "view").as[String] == "no-freeze")
      assert((contestReq(new Ctl(src), p: _*).only \ "view").as[String] == "no-freeze")
    }
  }

  test("contest: times, elapsed/remaining and problems in board order with names") {
    val src = frozenFake()
    val c = contestReq(new Ctl(src), pub: _*).only
    assert(c == Json.obj(
      "id" -> 1, "name" -> "Contest 1", "status" -> "running", "view" -> "frozen",
      "start" -> "2026-11-06T10:00:00Z", "end" -> "2026-11-06T13:00:00Z", "freeze" -> "2026-11-06T11:00:00Z",
      "elapsed_seconds" -> 7200, "remaining_seconds" -> 3600,
      "teams" -> teamsObj(2, 1),
      "problems" -> Json.arr(
        Json.obj("id" -> "A", "name" -> "Apples"),
        Json.obj("id" -> "B", "name" -> "Bananas"),
        Json.obj("id" -> "C", "name" -> "Cherries"))))
  }

  test("contest: no freeze configured gives freeze null") {
    val c = contestReq(new Ctl(fake(Seq(plainRunning), problems = Map(2 -> names))), pub: _*).only
    assert((c \ "freeze").get == play.api.libs.json.JsNull)
  }

  test("contest, all: no board falls back to the problem names, sorted by id") {
    val c = contestReq(new Ctl(fake(Seq(plainRunning), problems = Map(2 -> Map("B" -> "Bb", "A" -> "Aa")))), jury: _*).only
    assert((c \ "problems").get == Json.arr(Json.obj("id" -> "A", "name" -> "Aa"), Json.obj("id" -> "B", "name" -> "Bb")))
    assert((c \ "note").toOption.isEmpty, c)
  }

  test("contest, public: no board means no problems and a note, never the problem names") {
    for (running <- Seq(plainRunning, frozenRunning)) {
      val src = fake(Seq(running), problems = Map(running.id -> Map("B" -> "Bb", "A" -> "Aa")))
      val c = contestReq(new Ctl(src), pub: _*).only
      assert((c \ "problems").toOption.isEmpty, c)
      assert((c \ "note").as[String] == "board unavailable", c)
      assert(!c.toString.contains("Aa") && !c.toString.contains("Bb"), c)
      assert((c \ "start").isDefined && (c \ "remaining_seconds").isDefined, c)
    }
  }

  test("contest: a board problem without a name gets its id as name") {
    val c = contestReq(new Ctl(fake(Seq(plainRunning), Map(2 -> frozenBoard), problems = Map(2 -> Map("A" -> "Aa")))),
      pub: _*).only
    assert((c \ "problems").get == Json.arr(Json.obj("id" -> "A", "name" -> "Aa"), Json.obj("id" -> "B", "name" -> "B"),
      Json.obj("id" -> "C", "name" -> "C")))
  }

  test("before start: no problems, no rows, and no board or problem lookups") {
    val src = fake(Seq(upcoming), Map(3 -> frozenBoard), Map(3 -> realBoard), Map(3 -> names), Map(3 -> TeamCounts(7, 2)))
    for (p <- Seq(pub, jury)) {
      val c = contestReq(new Ctl(src), p: _*).only
      assert((c \ "status").as[String] == "before")
      assert((c \ "problems").toOption.isEmpty, c)
      assert((c \ "teams").get == teamsObj(7, 2), c)
      assert((c \ "elapsed_seconds").as[Int] == 0)
      assert((c \ "remaining_seconds").as[Int] == 4 * 3600)
      val s = standingsReq(new Ctl(src), p: _*).only
      assert((s \ "rows").toOption.isEmpty, s)
      assert((s \ "teams").toOption.isEmpty, s)
    }
    assert(!src.calls.exists(x => x.startsWith("board") || x.startsWith("problemNames")), src.calls)
    assert(src.calls.contains("teamCounts(3)"), src.calls)
  }

  test("contest: team counts for a running, a finished and an upcoming contest, the same for both audiences") {
    val running = frozenFake()
    for (p <- Seq(pub, jury)) assert((contestReq(new Ctl(running), p: _*).only \ "teams").get == teamsObj(2, 1))
    val src = betweenFake()
    for (p <- Seq(pub, jury)) {
      val cs = contestReq(new Ctl(src), p: _*).contests
      assert(cs.map(c => ((c \ "id").as[Int], (c \ "status").as[String], (c \ "teams").get)) ==
        Seq((3, "over", teamsObj(2, 1)), (4, "before", teamsObj(40, 3))))
    }
    // An empty participant list is reported as zeros, not omitted.
    val none = contestReq(new Ctl(fake(Seq(plainRunning))), pub: _*).only
    assert((none \ "teams").get == teamsObj(0, 0))
  }

  test("contest between contests: the finished one with problems, the upcoming one with times and teams only") {
    val src = betweenFake()
    val r = contestReq(new Ctl(src), pub: _*)
    info(r.body)
    assert(r.json == Json.obj("as_of" -> "2026-11-06T12:00:00Z", "contests" -> Json.arr(
      Json.obj("id" -> 3, "name" -> "Contest 3", "status" -> "over", "view" -> "no-freeze",
        "start" -> "2026-11-06T06:00:00Z", "end" -> "2026-11-06T10:00:00Z", "freeze" -> "2026-11-06T09:00:00Z",
        "elapsed_seconds" -> 14400, "remaining_seconds" -> 0, "teams" -> teamsObj(2, 1),
        "problems" -> Json.arr(Json.obj("id" -> "A", "name" -> "Apples"), Json.obj("id" -> "B", "name" -> "Bananas"),
          Json.obj("id" -> "C", "name" -> "Cherries"))),
      Json.obj("id" -> 4, "name" -> "Contest 4", "status" -> "before", "view" -> "no-freeze",
        "start" -> "2026-11-07T08:00:00Z", "end" -> "2026-11-07T13:00:00Z", "freeze" -> "2026-11-07T11:00:00Z",
        "elapsed_seconds" -> 0, "remaining_seconds" -> 18000, "teams" -> teamsObj(40, 3)))))
    assert(!src.calls.exists(x => x.startsWith("board(4") || x.startsWith("problemNames(4")), src.calls)
    assert(!src.calls.exists(_.contains("(1")), src.calls)
  }

  test("standings between contests: rows for the finished one, only the base object for the upcoming one") {
    val src = betweenFake()
    val r = standingsReq(new Ctl(src), pub :+ ("top" -> "2"): _*)
    info(r.body)
    assert(r.status == 200, r.body)
    val Seq(done, next) = r.contests
    assert((done \ "id").as[Int] == 3 && (done \ "status").as[String] == "over")
    assert((done \ "teams").as[Int] == 2)
    assert(rowTuples(done).size == 2)
    assert(next == Json.obj("id" -> 4, "name" -> "Contest 4", "status" -> "before", "view" -> "no-freeze"))
    assert(!src.calls.exists(_.startsWith("teamCounts")), src.calls)
    assert(src.calls.count(_.startsWith("board")) == 1, src.calls)
  }

  test("contest: a failed team count leaves out teams and answers the rest") {
    def failing(how: String) = new FakeSource(contestList = Seq(frozenRunning, upcoming.copy(id = 5)),
      freezingBoards = Map(1 -> frozenBoard), problems = Map(1 -> names)) {
      override def teamCounts(contestId: Int): Future[TeamCounts] =
        if (how == "throw") throw new IllegalStateException("boom")
        else Future.failed(new RuntimeException("secret sql detail"))
    }
    for (how <- Seq("throw", "fail"); src = failing(how)) {
      val r = contestReq(new Ctl(src), pub: _*)
      assert(r.status == 200, r.body)
      val c = r.only
      assert((c \ "teams").toOption.isEmpty, c)
      assert((c \ "problems").as[Seq[JsObject]].size == 3, c)
      assert((c \ "remaining_seconds").as[Int] == 3600, c)
      assert(!r.body.contains("secret") && !r.body.contains("boom"), r.body)
    }
    // An upcoming contest degrades the same way.
    val up = new FakeSource(contestList = Seq(upcoming)) {
      override def teamCounts(contestId: Int): Future[TeamCounts] = Future.failed(new RuntimeException("x"))
    }
    val c = contestReq(new Ctl(up), pub: _*).only
    assert((c \ "status").as[String] == "before" && (c \ "teams").toOption.isEmpty && (c \ "start").isDefined, c)
  }

  test("over: elapsed is the duration, remaining 0") {
    val over = contest(4, -5, -2, -1, 3)
    val c = contestReq(new Ctl(fake(Seq(over))), pub: _*).only
    assert((c \ "status").as[String] == "over")
    assert((c \ "view").as[String] == "frozen")
    assert((c \ "elapsed_seconds").as[Int] == 4 * 3600)
    assert((c \ "remaining_seconds").as[Int] == 0)
  }

  test("top: default 10, capped at 30, invalid values are 400") {
    val src = fake(Seq(plainRunning), Map(2 -> bigBoard))
    val d = standingsReq(new Ctl(src), pub: _*).only
    assert((d \ "rows").as[Seq[JsObject]].size == 10)
    assert((d \ "teams").as[Int] == 35)
    assert((standingsReq(new Ctl(src), pub :+ ("top" -> "5"): _*).only \ "rows").as[Seq[JsObject]].size == 5)
    assert((standingsReq(new Ctl(src), pub :+ ("top" -> "30"): _*).only \ "rows").as[Seq[JsObject]].size == 30)
    assert((standingsReq(new Ctl(src), pub :+ ("top" -> "100"): _*).only \ "rows").as[Seq[JsObject]].size == 30)
    for (bad <- Seq("0", "-1", "x", "", "99999999999")) {
      val r = standingsReq(new Ctl(src), pub :+ ("top" -> bad): _*)
      assert(r.status == 400, s"top=$bad: ${r.body}")
      assert((r.json \ "error").as[String].nonEmpty)
    }
  }

  test("two running contests: two elements, in id order") {
    val src = fake(Seq(plainRunning, frozenRunning, upcoming), Map(1 -> frozenBoard, 2 -> bigBoard))
    val c = contestReq(new Ctl(src), pub: _*)
    assert(c.contests.map(x => (x \ "id").as[Int]) == Seq(1, 2))
    val s = standingsReq(new Ctl(src), pub: _*)
    assert(s.contests.map(x => (x \ "id").as[Int]) == Seq(1, 2))
    assert(s.contests.map(x => (x \ "teams").as[Int]) == Seq(2, 35))
  }

  test("contests=11 with only disallowed contests: empty array and note") {
    val src = fake(Seq(contest(11, -1, 1, 1, 1), contest(12, -1, 1, 1, 1)), Map(11 -> bigBoard, 12 -> bigBoard))
    for (r <- Seq(contestReq(new Ctl(src), "audience" -> "public", "contests" -> "11"),
                  standingsReq(new Ctl(src), "audience" -> "all", "contests" -> "11"),
                  standingsReq(new Ctl(src), "audience" -> "all", "contests" -> "*"))) {
      assert(r.status == 200, r.body)
      assert((r.json \ "contests").as[Seq[JsObject]].isEmpty)
      assert((r.json \ "note").as[String] == "no contest")
      assert((r.json \ "as_of").as[String] == "2026-11-06T12:00:00Z")
    }
    assert(!src.calls.exists(_.startsWith("board")), src.calls)
  }

  test("contest=11 is 400, contest=x is 400; contest=2 narrows") {
    val src = fake(Seq(frozenRunning, plainRunning, contest(11, -1, 1, 1, 1)))
    for (bad <- Seq("11", "x", "", "3")) {
      val r = contestReq(new Ctl(src), pub :+ ("contest" -> bad): _*)
      assert(r.status == 400, s"contest=$bad: ${r.body}")
      assert((r.json \ "error").as[String].nonEmpty)
      assert(standingsReq(new Ctl(src), pub :+ ("contest" -> bad): _*).status == 400)
    }
    assert((contestReq(new Ctl(src), pub :+ ("contest" -> "2"): _*).only \ "id").as[Int] == 2)
  }

  test("missing or bad audience/contests: 400 with an error") {
    val src = frozenFake()
    for (p <- Seq(Seq("contests" -> "*"), Seq("audience" -> "public"), Seq("audience" -> "jury", "contests" -> "*"),
                  Seq("audience" -> "public", "contests" -> "a"), Seq.empty[(String, String)])) {
      for (r <- Seq(contestReq(new Ctl(src), p: _*), standingsReq(new Ctl(src), p: _*))) {
        assert(r.status == 400, s"$p: ${r.body}")
        assert((r.json \ "error").as[String].nonEmpty)
      }
    }
    assert(!src.calls.contains("contests"))
  }

  test("every success has as_of in ISO-8601 UTC") {
    val src = frozenFake()
    for (r <- Seq(contestReq(new Ctl(src), pub: _*), standingsReq(new Ctl(src), jury: _*))) {
      assert(r.status == 200)
      assert((r.json \ "as_of").as[String] == "2026-11-06T12:00:00Z")
    }
  }

  test("standings without a board: empty rows and a note") {
    val r = standingsReq(new Ctl(fake(Seq(plainRunning))), pub: _*)
    val c = r.only
    assert((c \ "rows").as[Seq[JsObject]].isEmpty)
    assert((c \ "note").as[String] == "board unavailable")
  }

  test("source failure: 500 with a generic error and no internals") {
    val failing = new FakeSource(contestList = Seq(frozenRunning)) {
      override def contests(): Future[Seq[Contest]] = Future.failed(new RuntimeException("secret sql detail"))
    }
    for (r <- Seq(contestReq(new Ctl(failing), pub: _*), standingsReq(new Ctl(failing), pub: _*))) {
      assert(r.status == 500)
      assert(r.json == Json.obj("error" -> "internal error"))
    }
    val failingBoard = new FakeSource(contestList = Seq(frozenRunning)) {
      override def problemNames(contestId: Int): Future[Map[String, String]] = throw new IllegalStateException("boom")
    }
    assert(contestReq(new Ctl(failingBoard), pub: _*).status == 500)
  }

  test("auth still applies: 401 without the token, 404 when disabled") {
    val src = frozenFake()
    val noAuth = RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget("/api/palevo/v1/contest?audience=public&contests=*", "/api/palevo/v1/contest",
        Map("audience" -> Seq("public"), "contests" -> Seq("*"))), "HTTP/1.1", Headers(), TypedMap.empty,
      AnyContentAsEmpty)
    assert(run(new Ctl(src).contest, noAuth).status == 401)
    assert(run(new Ctl(src).standings, noAuth).status == 401)
    assert(contestReq(new Ctl(src, tok = None), pub: _*).status == 404)
    assert(standingsReq(new Ctl(src, tok = None), pub: _*).status == 404)
    assert(src.calls.isEmpty)
  }
}
