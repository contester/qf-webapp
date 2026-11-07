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

class PalevoQueryTeamSpec extends AnyFunSuite with BeforeAndAfterAll {
  private val system = ActorSystem("PalevoQueryTeamSpec", ConfigFactory.parseString(
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
  private val sgu1 = team(11, "СГУ", 1, "Alpha: Ivanov, Petrov")
  private val msu1 = team(12, "МГУ", 1, "Beta: Sidorov")
  private val sgu2 = team(13, "СГУ", 2, "Gamma: Kuznetsov")
  private val other = team(21, "ТГУ", 1, "Delta: Smirnov")

  private val realBoard = LiveBoard.build(problemIds, Seq(sgu1, msu1, sgu2),
    Map(
      11 -> Map("A" -> Solved(2, 600), "B" -> Failed(3), "C" -> Pending(1)),
      12 -> Map("A" -> Solved(1, 300)),
      13 -> Map("A" -> Solved(1, 100))))
  // Freeze: team 11 solved A really, but under the freeze it shows as pending.
  private val frozenBoard = LiveBoard.build(problemIds, Seq(sgu1, msu1, sgu2),
    Map(11 -> Map("A" -> Pending(1)), 12 -> Map("A" -> Solved(1, 300)), 13 -> Map("A" -> Solved(1, 100))))
  private val board2 = LiveBoard.build(problemIds, Seq(other), Map(21 -> Map("A" -> Solved(1, 60))))

  private val frozenRunning = contest(1, -2, -1, 1, 2)
  private val plainRunning = contest(2, -2, 1, 1, 1)
  private val upcoming = contest(3, 1, 4, 5, 6)

  private class Ctl(source: QuerySource) extends PalevoQuery(cc, QueryConfig(Some(token), (1 to 10).toSet), source) {
    override protected def now(): DateTime = fixedNow
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

  private def teamReq(c: Ctl, params: (String, String)*) = {
    val res = await(c.team(req("/api/palevo/v1/team", params: _*)))
    Resp(res.header.status, await(res.body.consumeData).utf8String)
  }

  private val pub = Seq("audience" -> "public", "contests" -> "*")
  private val jury = Seq("audience" -> "all", "contests" -> "*")

  private def fake(contests: Seq[Contest] = Seq(frozenRunning), frozen: Map[Int, LiveBoard] = Map(1 -> frozenBoard),
                   real: Map[Int, LiveBoard] = Map(1 -> realBoard), rooms: Map[Int, Map[Int, String]] = Map.empty,
                   submits: Map[Int, Map[Int, DateTime]] = Map.empty) =
    new FakeSource(contestList = contests, freezingBoards = frozen, realBoards = real, rooms = rooms,
      submitTimes = submits)

  private def ids(c: JsObject) = (c \ "candidates").as[Seq[JsObject]].map(x => (x \ "id").as[Int])

  test("q=СГУ1 resolves one team with cells from the board") {
    val src = fake(submits = Map(1 -> Map(11 -> fixedNow.minusSeconds(90))))
    val r = teamReq(new Ctl(src), jury :+ ("q" -> "СГУ1"): _*)
    assert(r.status == 200, r.body)
    val c = r.only
    assert((c \ "id").as[Int] == 1)
    assert(ids(c) == Seq(11))
    val t = c \ "team"
    assert((t \ "id").as[Int] == 11)
    assert((t \ "rank").as[String] == "3")
    assert((t \ "solved").as[Int] == 1)
    assert((t \ "problems" \ "A" \ "state").as[String] == "ok")
    assert((t \ "problems" \ "A" \ "minutes").as[Int] == 10)
    assert((t \ "problems" \ "A" \ "attempts").as[Int] == 2)
    assert((t \ "problems" \ "B" \ "state").as[String] == "fail")
    assert((t \ "problems" \ "B" \ "attempts").as[Int] == 3)
    assert((t \ "problems" \ "B" \ "minutes").toOption.isEmpty)
    assert((t \ "problems" \ "C" \ "state").as[String] == "pending")
    assert((t \ "problems" \ "C" \ "attempts").as[Int] == 1)
    assert((t \ "problems" \ "D" \ "state").as[String] == "empty")
    assert((t \ "problems" \ "D" \ "attempts").as[Int] == 0)
    assert((t \ "last_submit_at").as[String] == "2026-11-06T11:58:30Z")
    assert((t \ "seconds_since_last_submit").as[Int] == 90)
  }

  test("two matches give candidates and no team (never guess)") {
    val r = teamReq(new Ctl(fake()), jury :+ ("q" -> "СГУ"): _*)
    val c = r.only
    assert(ids(c).toSet == Set(11, 13) && ids(c).size == 2)
    assert((c \ "team").toOption.isEmpty)
    val cands = (c \ "candidates").as[Seq[JsObject]]
    assert(cands.head.keys == Set("id", "name", "school_short", "num"))
    val first = cands.find(x => (x \ "id").as[Int] == 11).get
    assert((first \ "name").as[String] == sgu1.teamFullName)
    assert((first \ "school_short").as[String] == "СГУ")
    assert((first \ "num").as[Int] == 1)
    assert((cands.find(x => (x \ "id").as[Int] == 13).get \ "num").as[Int] == 2)
  }

  // A school with ten-plus teams: СГУ1 is an exact match of #1 and a prefix of #10..#12.
  private val sguMany = Seq(sgu1, sgu2) ++ Seq(10, 11, 12, 20).map(n => team(100 + n, "СГУ", n, s"T$n: Member$n"))
  private def manyFake(teams: Seq[Team]) = {
    val b = LiveBoard.build(problemIds, teams, Map.empty)
    fake(frozen = Map(1 -> b), real = Map(1 -> b))
  }

  test("a unique exact match resolves even when other teams match by prefix") {
    val src = manyFake(sguMany)
    val one = teamReq(new Ctl(src), jury :+ ("q" -> "СГУ1"): _*)
    assert(one.status == 200, one.body)
    assert(ids(one.only) == Seq(11), one.body)
    assert((one.only \ "team" \ "id").as[Int] == 11, one.body)
    val ten = teamReq(new Ctl(src), jury :+ ("q" -> "СГУ10"): _*)
    assert(ids(ten.only) == Seq(110) && (ten.only \ "team" \ "id").as[Int] == 110, ten.body)
    val two = teamReq(new Ctl(src), pub :+ ("q" -> "СГУ2"): _*)
    assert(ids(two.only) == Seq(13) && (two.only \ "team" \ "id").as[Int] == 13, two.body)
    val twoAlone = teamReq(new Ctl(manyFake(Seq(sgu1, sgu2, msu1))), jury :+ ("q" -> "СГУ2"): _*)
    assert(ids(twoAlone.only) == Seq(13) && (twoAlone.only \ "team" \ "id").as[Int] == 13, twoAlone.body)
  }

  test("several exact matches stay ambiguous") {
    val r = teamReq(new Ctl(manyFake(sguMany)), jury :+ ("q" -> "СГУ"): _*)
    assert(r.status == 200, r.body)
    assert(ids(r.only).size == 5, r.body)
    assert((r.only \ "team").toOption.isEmpty, r.body)
  }

  // Eight teams; the digit 1 occurs in several school+number keys, 46 in two, and no name is a bare number.
  private val numbered = Seq(
    team(31, "СГУ", 1, "Alpha: A"), team(32, "СГУ", 2, "Beta: B"), team(33, "МГУ", 1, "Gamma: C"),
    team(34, "МГУ", 11, "Delta: D"), team(35, "ТГУ", 12, "Eps: E"), team(36, "ТГУ", 46, "Zeta: F"),
    team(37, "КГУ", 46, "Eta: G"), team(38, "КГУ", 3, "Theta: H"), team(39, "ОмГУ", 21, "Iota: I"))

  test("a bare number never matches every team and never guesses") {
    val src = manyFake(numbered)
    for (q <- Seq("1", "46")) {
      val r = teamReq(new Ctl(src), jury :+ ("q" -> q): _*)
      assert(r.status == 200, r.body)
      val found = ids(r.only)
      assert(found.size <= 5, s"q=$q ${r.body}")
      assert(found.size < numbered.size, s"q=$q ${r.body}")
      assert(found.size > 1, s"q=$q ${r.body}")
      assert((r.only \ "team").toOption.isEmpty, s"q=$q ${r.body}")
    }
    assert(ids(teamReq(new Ctl(src), jury :+ ("q" -> "46"): _*).only).toSet == Set(36, 37))
  }

  test("a bare number resolves when it is exactly one team's own name") {
    val named1 = team(40, "ЯГУ", 7, "1: Solo")
    val r = teamReq(new Ctl(manyFake(numbered :+ named1)), jury :+ ("q" -> "1"): _*)
    assert(r.status == 200, r.body)
    assert(ids(r.only) == Seq(40), r.body)
    assert((r.only \ "team" \ "id").as[Int] == 40, r.body)
  }

  test("id works and a non-integer id is 400; neither q nor id is 400") {
    val r = teamReq(new Ctl(fake()), jury :+ ("id" -> "12"): _*)
    assert(r.status == 200, r.body)
    assert(ids(r.only) == Seq(12))
    assert((r.only \ "team" \ "id").as[Int] == 12)
    assert(teamReq(new Ctl(fake()), jury :+ ("id" -> "x"): _*).status == 400)
    val none = teamReq(new Ctl(fake()), jury: _*)
    assert(none.status == 400 && (none.json \ "error").isDefined)
  }

  test("id not in the board gives no candidates") {
    val r = teamReq(new Ctl(fake()), jury :+ ("id" -> "999"): _*)
    assert(r.status == 200 && ids(r.only).isEmpty && (r.only \ "team").toOption.isEmpty)
  }

  test("a team of a second contest is found there and the result names its contest") {
    val src = fake(Seq(frozenRunning, plainRunning), Map(1 -> frozenBoard, 2 -> board2), Map(1 -> realBoard, 2 -> board2))
    val r = teamReq(new Ctl(src), jury :+ ("q" -> "ТГУ1"): _*)
    assert(r.status == 200, r.body)
    val byId = r.contests.map(c => (c \ "id").as[Int] -> c).toMap
    assert(ids(byId(1)).isEmpty && (byId(1) \ "team").toOption.isEmpty)
    assert(ids(byId(2)) == Seq(21))
    assert((byId(2) \ "name").as[String] == "Contest 2")
    assert((byId(2) \ "team" \ "id").as[Int] == 21)
  }

  test("room: present for all when known, absent for public (and not even looked up), absent when unknown") {
    val rooms = Map(1 -> Map(11 -> "Room 5"))
    val srcAll = fake(rooms = rooms)
    val a = teamReq(new Ctl(srcAll), jury :+ ("q" -> "СГУ1"): _*)
    assert((a.only \ "team" \ "room").as[String] == "Room 5")
    val srcPub = fake(rooms = rooms)
    val p = teamReq(new Ctl(srcPub), pub :+ ("q" -> "СГУ1"): _*)
    assert((p.only \ "team" \ "id").as[Int] == 11)
    assert((p.only \ "team" \ "room").toOption.isEmpty)
    assert(!srcPub.calls.exists(_.startsWith("teamRooms")), srcPub.calls.toString)
    val u = teamReq(new Ctl(fake()), jury :+ ("q" -> "СГУ1"): _*)
    assert((u.only \ "team").toOption.isDefined && (u.only \ "team" \ "room").toOption.isEmpty)
  }

  test("hostile queries: no exception, no wide match, at most 5 candidates") {
    for (q <- Seq("", ".*", "%", "_", "a" * 5000, "'; drop table--")) {
      val r = teamReq(new Ctl(fake()), jury :+ ("q" -> q): _*)
      assert(r.status == 200, s"$q ${r.body}")
      assert(ids(r.only).size <= 5)
      assert(ids(r.only).size < 3, s"wide match for $q")
      assert((r.only \ "team").toOption.isEmpty || ids(r.only).size == 1)
    }
    assert(ids(teamReq(new Ctl(fake()), jury :+ ("q" -> ""): _*).only).isEmpty)
  }

  test("public with a freeze: the cell comes from the Freezing board") {
    val src = fake()
    val p = teamReq(new Ctl(src), pub :+ ("q" -> "СГУ1"): _*)
    assert((p.only \ "team" \ "problems" \ "A" \ "state").as[String] == "pending")
    assert(src.calls.contains("board(1,false)") && !src.calls.contains("board(1,true)"))
    val j = teamReq(new Ctl(fake()), jury :+ ("q" -> "СГУ1"): _*)
    assert((j.only \ "team" \ "problems" \ "A" \ "state").as[String] == "ok")
  }

  test("board unavailable: empty candidates and a note") {
    val r = teamReq(new Ctl(fake(frozen = Map.empty, real = Map.empty)), jury :+ ("q" -> "СГУ1"): _*)
    assert(ids(r.only).isEmpty && (r.only \ "note").as[String] == "board unavailable")
  }

  test("before the start: no candidates and the board is not read") {
    val src = fake(Seq(upcoming), Map(3 -> frozenBoard), Map(3 -> realBoard))
    val r = teamReq(new Ctl(src), jury :+ ("q" -> "СГУ1"): _*)
    assert(r.status == 200, r.body)
    assert(ids(r.only).isEmpty && (r.only \ "team").toOption.isEmpty)
    assert(!src.calls.exists(_.startsWith("board")))
  }
}
