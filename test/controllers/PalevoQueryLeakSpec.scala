package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.ConfigFactory
import live.{LiveBoard, LiveCell}
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, LocalTeam, Team}
import palevo.query._
import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.i18n.{DefaultLangs, DefaultMessagesApi}
import play.api.libs.json._
import play.api.libs.typedmap.TypedMap
import play.api.mvc._
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/** End-to-end matrix: every endpoint x audience x freeze state, checked for what must never leak. */
class PalevoQueryLeakSpec extends AnyFunSuite with BeforeAndAfterAll {
  private val system = ActorSystem("PalevoQueryLeakSpec", ConfigFactory.parseString(
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
  private def h(n: Double) = fixedNow.plusMinutes((n * 60).toInt)

  private def contest(id: Int, freeze: Double, expose: Double) =
    Contest(id, s"Contest $id", h(-2), h(freeze), h(1), h(expose), None, "en", schoolMode = false)

  private def team(id: Int, short: String, num: Int, name: String): Team =
    LocalTeam(id, 1, s"School $short", short, num, name, notRated = false, noPrint = false, disabled = false)

  import LiveCell._

  private val t1 = team(11, "SGU", 1, "Alpha")
  private val t2 = team(12, "MSU", 1, "Beta")
  private val t3 = team(13, "SGU", 2, "Gamma")
  private val r1 = team(14, "RealOnly", 1, "RealOnlyOne")
  private val r2 = team(15, "RealOnly", 2, "RealOnlyTwo")

  private val publicBoard = LiveBoard.build(Seq("A", "B"), Seq(t1, t2, t3), Map(
    11 -> Map("A" -> Pending(1)), 12 -> Map("A" -> Solved(1, 300)), 13 -> Map("B" -> Failed(2))))
  // Distinctive: five teams and an extra problem that only the real board has.
  private val realBoard = LiveBoard.build(Seq("A", "B", "Z"), Seq(t1, t2, t3, r1, r2), Map(
    11 -> Map("A" -> Solved(1, 100)), 12 -> Map("A" -> Solved(1, 300)), 13 -> Map("B" -> Failed(2)),
    14 -> Map("Z" -> Solved(1, 50)), 15 -> Map("Z" -> Solved(2, 60))))
  private val names = Map("A" -> "Apples", "B" -> "Bananas", "Z" -> "RealOnlyProblem")

  private val RealMarkers = Seq("RealOnly", "\"teams\":5", "\"Z\"")
  /** Keys only the jury ever gets. `/computers` and `/printing` are public too, minus what the public never sees: */
  private val JuryKeys = Set("room")
  /** the print agent's name, ... */
  private val PublicForbiddenKeys = Set("agent")
  /** ... and a computer's raw address (a print job from a computer missing from the location table). */
  private val Addresses = Seq("10.20.30.40", "fe80::1")
  private def looksLikeAddress(s: String) = s.matches("[0-9.]+(/[0-9]+)?") || s.contains(":")

  private sealed abstract class Freeze(val label: String, val freeze: Double, val expose: Double)
  private case object NoFreeze0 extends Freeze("none", 1, 2)       // freeze time not before the end
  private case object InEffect extends Freeze("in effect", -1, 2)  // frozen now
  private case object Exposed extends Freeze("exposed", -1.5, -0.5) // freeze over, results exposed
  private val freezes = Seq(NoFreeze0, InEffect, Exposed)

  private def source(f: Freeze) = new FakeSource(
    contestList = Seq(contest(1, f.freeze, f.expose), contest(11, f.freeze, f.expose)),
    freezingBoards = Map(1 -> publicBoard, 11 -> publicBoard),
    realBoards = Map(1 -> realBoard, 11 -> realBoard),
    problems = Map(1 -> names, 11 -> names),
    submitTimes = Map(1 -> Map(11 -> fixedNow.minusMinutes(3)), 11 -> Map(11 -> fixedNow.minusMinutes(3))),
    arrivals = Seq((1, "A", fixedNow.minusMinutes(2)), (11, "A", fixedNow.minusMinutes(2))),
    pending = Map(1 -> Map("A" -> 1), 11 -> Map("A" -> 1)),
    rooms = Map(1 -> Map(11 -> "Room 7"), 11 -> Map(11 -> "Room 7")),
    computerList = Seq(ComputerRow("wi46", "Windows room", "HP-1"), ComputerRow("lin46", "Linux room", "HP-2")),
    jobs = Seq(PrintJobRow(1, 1, 11, "wi46", "Windows room", printed = true, "", 3, fixedNow.minusMinutes(5)),
      PrintJobRow(2, 11, 11, "wi46", "Windows room", printed = true, "", 3, fixedNow.minusMinutes(5)),
      PrintJobRow(3, 1, 12, Addresses(0), "", printed = false, "", 1, fixedNow.minusMinutes(6)),
      PrintJobRow(4, 1, 13, Addresses(1), "", printed = false, "jam", 2, fixedNow.minusMinutes(7))),
    printers = Seq(PrinterProblem("agent1", "HP-1", "paper out")),
    counts = Map(1 -> TeamCounts(4, 1), 11 -> TeamCounts(77, 7)))

  private class Ctl(token: Option[String], src: QuerySource)
    extends PalevoQuery(cc, QueryConfig(token, (1 to 10).toSet), src) {
    override protected def now(): DateTime = fixedNow
  }

  private def req(path: String, auth: Boolean, params: Seq[(String, String)]): Request[AnyContent] = {
    val q = params.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }
    val uri = path + (if (params.isEmpty) "" else params.map { case (k, v) => s"$k=$v" }.mkString("?", "&", ""))
    RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget(uri, path, q), "HTTP/1.1",
      Headers((if (auth) Seq("Authorization" -> s"Bearer $token") else Seq.empty[(String, String)]): _*), TypedMap.empty, AnyContentAsEmpty)
  }

  private def await[T](f: Future[T]): T = Await.result(f, 5.seconds)

  private case class Resp(status: Int, body: String) { lazy val json: JsValue = Json.parse(body) }

  /** (name, action, extra parameters, contest-bound). Every endpoint answers both audiences. */
  private case class Endpoint(name: String, run: (PalevoQuery, Request[AnyContent]) => Future[Result],
                              extra: Seq[(String, String)], bound: Boolean)
  private val endpoints = Seq(
    Endpoint("contest", _.contest(_), Nil, bound = true),
    Endpoint("standings", _.standings(_), Nil, bound = true),
    Endpoint("team", _.team(_), Seq("q" -> "Alpha"), bound = true),
    Endpoint("problems", _.problems(_), Nil, bound = true),
    Endpoint("computers", _.computers(_), Seq("q" -> "46"), bound = false),
    Endpoint("printing", _.printing(_), Nil, bound = true))

  private def call(e: Endpoint, c: PalevoQuery, auth: Boolean, params: Seq[(String, String)]): Resp = {
    val res = await(e.run(c, req(s"/api/palevo/v1/${e.name}", auth, params)))
    Resp(res.header.status, await(res.body.consumeData).utf8String)
  }

  private def keys(v: JsValue): Set[String] = v match {
    case o: JsObject => o.fields.flatMap { case (k, x) => keys(x) + k }.toSet
    case a: JsArray => a.value.flatMap(keys).toSet
    case _ => Set.empty
  }

  /** Every value of a `computer` field anywhere in `v`. */
  private def computerValues(v: JsValue): Seq[String] = v match {
    case o: JsObject => o.fields.flatMap {
      case ("computer", JsString(s)) => Seq(s)
      case (_, x) => computerValues(x)
    }
    case a: JsArray => a.value.flatMap(computerValues)
    case _ => Nil
  }

  private def expectedView(f: Freeze, audience: String) =
    if (f == InEffect) (if (audience == "public") "frozen" else "live-jury") else "no-freeze"

  for (f <- freezes; audience <- Seq("public", "all"); e <- endpoints) {
    test(s"${e.name} / $audience / freeze ${f.label}") {
      val src = source(f)
      val r = call(e, new Ctl(Some(token), src), auth = true,
        Seq("audience" -> audience, "contests" -> "*") ++ e.extra)
      assert(r.status == 200, r.body)
      if (e.bound) {
        val cs = (r.json \ "contests").as[Seq[JsObject]]
        assert(cs.nonEmpty)
        cs.foreach(c => assert((c \ "view").as[String] == expectedView(f, audience), r.body))
      }
      if (audience == "public") {
        assert(keys(r.json).intersect(JuryKeys ++ PublicForbiddenKeys).isEmpty, r.body)
        computerValues(r.json).foreach(v => assert(!looksLikeAddress(v), r.body))
        Addresses.foreach(a => assert(!r.body.contains(a), r.body))
        assert(!src.calls.exists(_.startsWith("teamRooms")) && src.calls.forall(!_.contains("true")),
          src.calls.toString)
        if (f == InEffect) RealMarkers.foreach(m => assert(!r.body.contains(m), s"$m in ${r.body}"))
      }
    }
  }

  for (f <- freezes; audience <- Seq("public", "all"); e <- endpoints.filter(_.bound);
       scope <- Seq("*", "1,11")) {
    test(s"contest 11 is never mentioned: ${e.name} / $audience / freeze ${f.label} / contests=$scope") {
      val src = source(f)
      val r = call(e, new Ctl(Some(token), src), auth = true,
        Seq("audience" -> audience, "contests" -> scope) ++ e.extra)
      assert(r.status == 200, r.body)
      assert((r.json \ "contests").as[Seq[JsObject]].map(c => (c \ "id").as[Int]) == Seq(1), r.body)
      assert(!r.body.contains("Contest 11"), r.body)
      assert(src.calls.forall(c => !c.contains("(11")), src.calls.toString)
    }
  }

  for (f <- freezes) test(s"contest: team counts are public and the same for both audiences / freeze ${f.label}") {
    val teams = Seq("public", "all").map { audience =>
      val r = call(endpoints.head, new Ctl(Some(token), source(f)), auth = true,
        Seq("audience" -> audience, "contests" -> "*"))
      if (audience == "public" && f == InEffect) info(r.body)
      (r.json \ "contests" \ 0 \ "teams").get
    }
    assert(teams.distinct == Seq(Json.obj("regular" -> 4, "out_of_competition" -> 1, "total" -> 5)), teams)
  }

  for (audience <- Seq("public", "all")) test(s"printing never returns a job of contest 11 / $audience") {
    val r = call(endpoints.last, new Ctl(Some(token), source(NoFreeze0)), auth = true,
      Seq("audience" -> audience, "contests" -> "*"))
    val ids = (r.json \ "contests").as[Seq[JsObject]].flatMap(c => (c \ "print_jobs").as[Seq[JsObject]])
      .map(j => (j \ "id").as[Int])
    assert(ids == Seq(1, 3, 4), r.body)
  }

  test("the jury still gets the agent and the addresses") {
    val r = call(endpoints.last, new Ctl(Some(token), source(InEffect)), auth = true,
      Seq("audience" -> "all", "contests" -> "*"))
    assert(keys(r.json).contains("agent"), r.body)
    assert(computerValues(r.json).toSet == Set("wi46") ++ Addresses, r.body)
  }

  test("token unset: every route is 404 with an empty body, with or without a bearer header") {
    for (e <- endpoints; auth <- Seq(true, false)) {
      val r = call(e, new Ctl(None, source(InEffect)), auth, Seq("audience" -> "all", "contests" -> "*") ++ e.extra)
      assert(r.status == 404 && r.body.isEmpty, s"${e.name}: ${r.status} ${r.body}")
    }
  }

  test("token set: no header is 401 on every route and nothing is read") {
    for (e <- endpoints) {
      val src = source(InEffect)
      val r = call(e, new Ctl(Some(token), src), auth = false, Seq("audience" -> "all", "contests" -> "*") ++ e.extra)
      assert(r.status == 401 && !r.body.contains(token), e.name)
      assert(src.calls.isEmpty)
    }
  }
}
