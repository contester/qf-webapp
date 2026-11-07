package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.ConfigFactory
import live.{LiveBoard, LiveCell}
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, LocalTeam, Team}
import palevo.query.{FakeSource, QueryConfig, QuerySource}
import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.i18n.{DefaultLangs, DefaultMessagesApi}
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.typedmap.TypedMap
import play.api.mvc._
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class PalevoQueryProblemsSpec extends AnyFunSuite with BeforeAndAfterAll {
  private val system = ActorSystem("PalevoQueryProblemsSpec", ConfigFactory.parseString(
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

  private def team(id: Int, short: String, num: Int, name: String): Team =
    LocalTeam(id, 1, s"School $short", short, num, name, notRated = false, noPrint = false, disabled = false)

  import LiveCell._

  private val problemIds = Seq("A", "B", "C", "D")
  private val names = Map("A" -> "Apples", "B" -> "Bananas", "C" -> "Cherries", "D" -> "Dates")
  private val t1 = team(11, "SGU", 1, "Alpha")
  private val t2 = team(12, "MSU", 1, "Beta")
  private val t3 = team(13, "SGU", 2, "Gamma")
  private val teams = Seq(t1, t2, t3)

  private val realBoard = LiveBoard.build(problemIds, teams, Map(
    11 -> Map("A" -> Solved(2, 600), "B" -> Failed(3), "C" -> Pending(1)),
    12 -> Map("A" -> Solved(1, 300), "B" -> Failed(1)),
    13 -> Map("A" -> Solved(1, 100))))
  // Under the freeze team 11's solve of A is hidden and shows as pending.
  private val frozenBoard = LiveBoard.build(problemIds, teams, Map(
    11 -> Map("A" -> Pending(1), "B" -> Failed(3), "C" -> Pending(1)),
    12 -> Map("A" -> Solved(1, 300), "B" -> Failed(1)),
    13 -> Map("A" -> Solved(1, 100))))

  private val frozenRunning = contest(1, -2, -1, 1, 2)
  private val upcoming = contest(3, 1, 4, 5, 6)

  private class Ctl(source: QuerySource) extends PalevoQuery(cc, QueryConfig(Some(token), (1 to 10).toSet), source) {
    override protected def now(): DateTime = fixedNow
    // The degraded-counts tests fail lookups on purpose; keep their warnings out of the test output.
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
    def only: JsObject = { val cs = (json \ "contests").as[Seq[JsObject]]; assert(cs.size == 1, body); cs.head }
    def problems: Map[String, JsObject] = (only \ "problems").as[Seq[JsObject]].map(p => (p \ "id").as[String] -> p).toMap
  }

  private def get(src: QuerySource, params: (String, String)*) = {
    val res = await(new Ctl(src).problems(req("/api/palevo/v1/problems", params: _*)))
    Resp(res.header.status, await(res.body.consumeData).utf8String)
  }

  private val pub = Seq("audience" -> "public", "contests" -> "*")
  private val jury = Seq("audience" -> "all", "contests" -> "*")

  private def fake(contests: Seq[Contest] = Seq(frozenRunning), frozen: Map[Int, LiveBoard] = Map(1 -> frozenBoard),
                   real: Map[Int, LiveBoard] = Map(1 -> realBoard),
                   arrivals: Seq[(Int, String, DateTime)] = Nil, pending: Map[Int, Map[String, Int]] = Map.empty) =
    new FakeSource(contestList = contests, freezingBoards = frozen, realBoards = real,
      problems = Map(1 -> names, 3 -> names), arrivals = arrivals, pending = pending)

  private def ints(p: JsObject) =
    Seq("solved", "teams_attempted", "attempts", "recent_submits", "pending").map(k => (p \ k).as[Int])

  private val arrivals = Seq((1, "A", fixedNow.minusMinutes(5)), (1, "A", fixedNow.minusMinutes(10)),
    (1, "B", fixedNow.minusMinutes(14)), (1, "B", fixedNow.minusMinutes(30)))
  private val pend = Map(1 -> Map("A" -> 1, "C" -> 2))

  test("counts from a board with solved, failed, pending and empty cells (jury)") {
    val r = get(fake(arrivals = arrivals, pending = pend), jury: _*)
    assert(r.status == 200, r.body)
    assert((r.only \ "window_minutes").as[Int] == 15)
    assert((r.only \ "problems").as[Seq[JsObject]].map(p => (p \ "id").as[String]) == problemIds)
    assert((r.problems("A") \ "name").as[String] == "Apples")
    // solved, teams_attempted, attempts, recent_submits, pending
    assert(ints(r.problems("A")) == Seq(3, 3, 4, 2, 1))
    assert(ints(r.problems("B")) == Seq(0, 2, 4, 1, 0))
    assert(ints(r.problems("C")) == Seq(0, 1, 1, 0, 2))
    assert(ints(r.problems("D")) == Seq(0, 0, 0, 0, 0))
  }

  test("default window is 15 minutes and the since passed to the source is now - window") {
    val src = fake(arrivals = arrivals)
    get(src, jury: _*)
    // arrival exactly 14 minutes ago counts, the 30 minute old one does not
    val r = get(src, jury :+ ("window" -> "20m"): _*)
    assert((r.only \ "window_minutes").as[Int] == 20)
    assert((r.problems("B") \ "recent_submits").as[Int] == 1)
    val r2 = get(src, jury :+ ("window" -> "31m"): _*)
    assert((r2.problems("B") \ "recent_submits").as[Int] == 2)
    val r3 = get(src, jury :+ ("window" -> "5m"): _*)
    assert((r3.problems("A") \ "recent_submits").as[Int] == 1)
    val r4 = get(src, jury :+ ("window" -> "4m"): _*)
    assert((r4.problems("A") \ "recent_submits").as[Int] == 0)
  }

  test("default window passes now - 15 minutes to recentArrivals") {
    var seen: Option[DateTime] = None
    val base = fake()
    val src = new QuerySource {
      def contests() = base.contests()
      def board(c: Int, real: Boolean) = base.board(c, real)
      def teamCounts(c: Int) = base.teamCounts(c)
      def problemNames(c: Int) = base.problemNames(c)
      def lastSubmits(c: Int, t: Seq[Int]) = base.lastSubmits(c, t)
      def recentArrivals(c: Int, since: DateTime) = { seen = Some(since); base.recentArrivals(c, since) }
      def pendingCounts(c: Int) = base.pendingCounts(c)
      def teamRooms(c: Int, t: Seq[Int]) = base.teamRooms(c, t)
      def computers() = base.computers()
      def printJobs(c: Seq[Int], t: Option[Int], a: Option[String], l: Int) = base.printJobs(c, t, a, l)
      def printerProblems() = base.printerProblems()
    }
    assert(get(src, jury: _*).status == 200)
    assert(seen.contains(fixedNow.minusMinutes(15)))
  }

  for (bad <- Seq("300m", "0m", "121m", "15", "x", "", "-5m", "1.5m", "m")) {
    test(s"window='$bad' is a 400 with no source access") {
      val src = fake()
      val r = get(src, jury :+ ("window" -> bad): _*)
      assert(r.status == 400, r.body)
      assert((r.json \ "error").asOpt[String].isDefined)
      assert(src.calls.isEmpty, src.calls.toString)
    }
  }

  test("window bounds 1m and 120m are accepted") {
    assert(get(fake(), jury :+ ("window" -> "1m"): _*).status == 200)
    assert(get(fake(), jury :+ ("window" -> "120m"): _*).status == 200)
  }

  test("public freeze hides the post-freeze solve from solved/attempts; jury counts it; activity is the same") {
    val src = fake(arrivals = arrivals, pending = pend)
    val p = get(src, pub: _*)
    val j = get(src, jury: _*)
    assert(ints(p.problems("A")) == Seq(2, 3, 3, 2, 1))
    assert(ints(j.problems("A")) == Seq(3, 3, 4, 2, 1))
    for (id <- problemIds) {
      assert((p.problems(id) \ "recent_submits") == (j.problems(id) \ "recent_submits"))
      assert((p.problems(id) \ "pending") == (j.problems(id) \ "pending"))
    }
    assert(src.calls.contains("board(1,false)") && src.calls.contains("board(1,true)"))
  }

  test("before start: base object only, no board or activity reads") {
    val src = fake(contests = Seq(upcoming))
    val r = get(src, jury: _*)
    assert(r.status == 200, r.body)
    assert(!r.only.keys.contains("problems"))
    assert(src.calls == Seq("contests"), src.calls.toString)
  }

  test("board unavailable gives empty problems and a note") {
    val r = get(fake(frozen = Map.empty), pub: _*)
    assert(r.status == 200, r.body)
    assert((r.only \ "problems").as[Seq[JsObject]].isEmpty)
    assert((r.only \ "note").as[String] == "board unavailable")
  }

  test("a failing recentArrivals or pendingCounts degrades to zeros, still 200") {
    val base = fake(pending = pend, arrivals = arrivals)
    val src = new QuerySource {
      def contests() = base.contests()
      def board(c: Int, real: Boolean) = base.board(c, real)
      def teamCounts(c: Int) = base.teamCounts(c)
      def problemNames(c: Int) = base.problemNames(c)
      def lastSubmits(c: Int, t: Seq[Int]) = base.lastSubmits(c, t)
      def recentArrivals(c: Int, since: DateTime) = Future.failed(new RuntimeException("db down"))
      def pendingCounts(c: Int) = throw new RuntimeException("sync boom")
      def teamRooms(c: Int, t: Seq[Int]) = base.teamRooms(c, t)
      def computers() = base.computers()
      def printJobs(c: Seq[Int], t: Option[Int], a: Option[String], l: Int) = base.printJobs(c, t, a, l)
      def printerProblems() = base.printerProblems()
    }
    val r = get(src, jury: _*)
    assert(r.status == 200, r.body)
    assert(ints(r.problems("A")) == Seq(3, 3, 4, 0, 0))
  }
}
