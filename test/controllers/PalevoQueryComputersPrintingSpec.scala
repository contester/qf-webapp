package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.ConfigFactory
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest
import palevo.query._
import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.i18n.{DefaultLangs, DefaultMessagesApi}
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.typedmap.TypedMap
import play.api.mvc._
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class PalevoQueryComputersPrintingSpec extends AnyFunSuite with BeforeAndAfterAll {
  private val system = ActorSystem("PalevoQueryComputersPrintingSpec", ConfigFactory.parseString(
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

  private def contest(id: Int) = Contest(id, s"Contest $id", h(-2), h(-1), h(1), h(2), None, "en", schoolMode = false)

  /** Remembers the contest lists `printJobs` was asked for, and the other arguments. */
  private class Spy(inner: FakeSource) extends QuerySource {
    val asked = new java.util.concurrent.ConcurrentLinkedQueue[(Seq[Int], Option[Int], Option[String], Int)]()
    def contests() = inner.contests()
    def board(c: Int, real: Boolean) = inner.board(c, real)
    def teamCounts(c: Int) = inner.teamCounts(c)
    def problemNames(c: Int) = inner.problemNames(c)
    def lastSubmits(c: Int, t: Seq[Int]) = inner.lastSubmits(c, t)
    def recentArrivals(c: Int, s: DateTime) = inner.recentArrivals(c, s)
    def pendingCounts(c: Int) = inner.pendingCounts(c)
    def teamRooms(c: Int, t: Seq[Int]) = inner.teamRooms(c, t)
    def computers() = inner.computers()
    def printJobs(cs: Seq[Int], t: Option[Int], a: Option[String], l: Int) = {
      asked.add((cs, t, a, l)); inner.printJobs(cs, t, a, l)
    }
    def printerProblems() = inner.printerProblems()
    def calls = inner.calls
  }

  private class Ctl(source: QuerySource) extends PalevoQuery(cc, QueryConfig(Some(token), Set(1, 2)), source) {
    override protected def now(): DateTime = fixedNow
    // The 500 test fails on purpose; keep its stack trace out of the test output.
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

  private case class Resp(status: Int, body: String) { lazy val json: JsValue = Json.parse(body) }

  private def computers(src: QuerySource, params: (String, String)*) = {
    val r = req("/api/palevo/v1/computers", params: _*)
    val res = await(new Ctl(src).computers(r))
    Resp(res.header.status, await(res.body.consumeData).utf8String)
  }
  private def printing(src: QuerySource, params: (String, String)*) = {
    val r = req("/api/palevo/v1/printing", params: _*)
    val res = await(new Ctl(src).printing(r))
    Resp(res.header.status, await(res.body.consumeData).utf8String)
  }

  private val pcs = Seq(
    ComputerRow("wi46", "Windows room", "HP-1"), ComputerRow("lin46", "Linux room", "HP-2"),
    ComputerRow("wi47", "Windows room", "HP-1"), ComputerRow("wi01", "Windows room", "HP-1"),
    ComputerRow("lin47", "Linux room", "HP-2"))
  private def job(id: Int, c: Int, team: Int, area: String = "Windows room", mins: Int = 0) =
    PrintJobRow(id, c, team, "wi46", area, printed = true, error = "", pages = 3, arrived = fixedNow.minusMinutes(mins + id))
  private val jobs = Seq(job(1, 1, 5), job(2, 1, 6, "Linux room"), job(3, 11, 5)) ++ (10 to 30).map(i => job(i, 2, 7))
  private val problemsList = Seq(PrinterProblem("agent1", "HP-1", "paper out"))
  private def fake() = new FakeSource(contestList = Seq(contest(1), contest(2), contest(11)), computerList = pcs,
    jobs = jobs, printers = problemsList)

  private val jury = Seq("audience" -> "all", "contests" -> "*")

  private def names(r: Resp) = (r.json \ "computers").as[Seq[JsObject]].map(c => (c \ "name").as[String])

  private val public = Seq("audience" -> "public", "contests" -> "*")

  test("/computers: public gets the same answer as the jury") {
    for (p <- Seq("q" -> "46", "q" -> "wi46", "area" -> "wINDOWS room", "area" -> "nope", "q" -> "x" * 5000)) {
      val pub = computers(fake(), public :+ p: _*)
      val all = computers(fake(), jury :+ p: _*)
      assert(pub.status == 200, pub.body)
      assert(pub.json == all.json, s"$p: ${pub.body} vs ${all.body}")
      (pub.json \ "computers").as[Seq[JsObject]].foreach(c => assert(c.keys == Set("name", "area", "printer"), pub.body))
    }
    assert(names(computers(fake(), public :+ ("q" -> "46"): _*)) == Seq("lin46", "wi46"))
    assert(computers(fake(), "audience" -> "public", "q" -> "46").status == 200)
    assert(computers(fake(), "audience" -> "public", "contests" -> "x", "q" -> "46").status == 400)
    assert(computers(fake(), public: _*).status == 400)
  }

  // Contest 1 also has jobs from computers missing from the location table: the source then gives their address.
  private val addressJobs = Seq(
    job(40, 1, 5).copy(computer = "10.20.30.40", area = ""),
    job(41, 1, 5).copy(computer = "fe80::1", area = ""),
    job(42, 1, 5).copy(computer = "192.168.0.7/32", area = ""))
  private def fakeWithAddresses() = new FakeSource(contestList = Seq(contest(1), contest(2), contest(11)),
    computerList = pcs, jobs = jobs ++ addressJobs, printers = problemsList)

  private def allJobs(r: Resp) = (r.json \ "contests").as[Seq[JsObject]].flatMap(c => (c \ "print_jobs").as[Seq[JsObject]])

  test("/printing public: no agent, no raw address, jobs of the selected contests only") {
    val spy = new Spy(fakeWithAddresses())
    val r = printing(spy, public: _*)
    assert(r.status == 200, r.body)
    val cs = (r.json \ "contests").as[Seq[JsObject]]
    assert(cs.map(c => (c \ "id").as[Int]) == Seq(1, 2))
    assert(cs.forall(c => (c \ "view").as[String] == "frozen"), r.body)
    val printers = (r.json \ "printers").as[Seq[JsObject]]
    assert(printers == Seq(Json.obj("printer" -> "HP-1", "condition" -> "paper out")), r.body)
    assert(!r.body.contains("agent"), r.body)
    val js = allJobs(r)
    assert(!js.map(j => (j \ "id").as[Int]).contains(3))
    js.foreach(j => assert(j.keys == Set("id", "team", "computer", "area", "printed", "error", "pages", "arrived"), r.body))
    val byId = js.map(j => (j \ "id").as[Int] -> j).toMap
    assert(Seq(40, 41, 42).forall(i => (byId(i) \ "computer").as[String] == ""), r.body)
    assert((byId(1) \ "computer").as[String] == "wi46" && (byId(1) \ "area").as[String] == "Windows room")
    assert((byId(40) \ "pages").as[Int] == 3 && (byId(40) \ "printed").as[Boolean])
    Seq("10.20.30.40", "fe80", "192.168").foreach(a => assert(!r.body.contains(a), r.body))
    val asked = spy.asked.toArray.toSeq.asInstanceOf[Seq[(Seq[Int], Option[Int], Option[String], Int)]]
    assert(asked.map(_._1.head).sorted == Seq(1, 2) && asked.forall(_._4 == 10))
  }

  test("/printing jury keeps agent and addresses") {
    val r = printing(fakeWithAddresses(), jury: _*)
    assert(r.status == 200, r.body)
    assert((r.json \ "printers").as[Seq[JsObject]] ==
      Seq(Json.obj("agent" -> "agent1", "printer" -> "HP-1", "condition" -> "paper out")), r.body)
    val byId = allJobs(r).map(j => (j \ "id").as[Int] -> (j \ "computer").as[String]).toMap
    assert(byId(40) == "10.20.30.40" && byId(41) == "fe80::1" && byId(42) == "192.168.0.7/32", r.body)
  }

  test("/printing public: contests=11 never reaches the source, filters and validation as for the jury") {
    val spy = new Spy(fake())
    val r = printing(spy, "audience" -> "public", "contests" -> "11")
    assert(r.status == 200, r.body)
    assert(spy.asked.isEmpty, spy.asked.toString)
    assert((r.json \ "note").as[String] == "no contest")
    assert((r.json \ "printers").as[Seq[JsObject]].size == 1, r.body)
    val f = printing(new Spy(fake()), public ++ Seq("team" -> "6", "area" -> "linux room"): _*)
    assert(allJobs(f).map(j => (j \ "id").as[Int]) == Seq(2), f.body)
    assert(printing(fake(), public :+ ("team" -> "x"): _*).status == 400)
    assert(printing(fake(), "audience" -> "public").status == 400)
    assert(printing(fake(), public :+ ("contest" -> "11"): _*).status == 400)
  }

  test("invalid or missing audience is 400") {
    assert(computers(fake(), "audience" -> "x", "q" -> "46").status == 400)
    assert(computers(fake(), "q" -> "46").status == 400)
    assert(printing(fake(), "audience" -> "x", "contests" -> "*").status == 400)
    assert(printing(fake(), "contests" -> "*").status == 400)
  }

  test("/computers q=46 finds wi46 and lin46; ambiguous across areas") {
    val r = computers(fake(), jury :+ ("q" -> "46"): _*)
    assert(r.status == 200, r.body)
    assert(names(r) == Seq("lin46", "wi46"))
    assert((r.json \ "ambiguous").as[Boolean])
    assert((r.json \ "contests").toOption.isEmpty)
    assert((r.json \ "as_of").as[String].nonEmpty)
    val wi = (r.json \ "computers").as[Seq[JsObject]].last
    assert((wi \ "area").as[String] == "Windows room" && (wi \ "printer").as[String] == "HP-1")
  }

  test("/computers not ambiguous when one computer or all share an area") {
    val one = computers(fake(), jury :+ ("q" -> "wi46"): _*)
    assert(names(one) == Seq("wi46") && !(one.json \ "ambiguous").as[Boolean])
    val src = new FakeSource(computerList = Seq(ComputerRow("wi46", "A", "p"), ComputerRow("wa46", "A", "p")))
    val r = computers(src, jury :+ ("q" -> "46"): _*)
    assert(names(r) == Seq("wa46", "wi46") && !(r.json \ "ambiguous").as[Boolean])
  }

  test("/computers area lists the room, ignoring case") {
    val r = computers(fake(), jury :+ ("area" -> "wINDOWS room"): _*)
    assert(r.status == 200, r.body)
    assert(names(r) == Seq("wi01", "wi46", "wi47"))
    assert(!(r.json \ "ambiguous").as[Boolean])
    assert(names(computers(fake(), jury :+ ("area" -> "nope"): _*)).isEmpty)
  }

  test("/computers works without contests, rejects malformed contests") {
    val r = computers(fake(), "audience" -> "all", "q" -> "46")
    assert(r.status == 200, r.body)
    assert(names(r) == Seq("lin46", "wi46"))
    assert(computers(fake(), "audience" -> "all", "contests" -> "x", "q" -> "46").status == 400)
  }

  test("/computers hostile q gives an empty list") {
    for (q <- Seq("", ".*", "%", "x" * 5000, "!!!--", "  ")) {
      val r = computers(fake(), jury :+ ("q" -> q): _*)
      assert(r.status == 200, q + r.body)
      assert(names(r).isEmpty, q + r.body)
      assert(!(r.json \ "ambiguous").as[Boolean])
    }
  }

  test("/computers needs q or area") {
    assert(computers(fake(), jury: _*).status == 400)
  }

  test("/computers answers at most 10") {
    val many = (1 to 30).map(i => ComputerRow(f"wi$i%02d", "A", "p"))
    val r = computers(new FakeSource(computerList = many), jury :+ ("area" -> "A"): _*)
    assert(names(r).size == 10)
  }

  test("/printing: printer problems and jobs of selected contests only") {
    val spy = new Spy(fake())
    val r = printing(spy, jury: _*)
    assert(r.status == 200, r.body)
    val cs = (r.json \ "contests").as[Seq[JsObject]]
    assert(cs.map(c => (c \ "id").as[Int]) == Seq(1, 2))
    val ids = cs.flatMap(c => (c \ "print_jobs").as[Seq[JsObject]].map(j => (j \ "id").as[Int]))
    assert(!ids.contains(3))
    assert(((cs.head \ "print_jobs").as[Seq[JsObject]]).map(j => (j \ "id").as[Int]) == Seq(1, 2))
    assert(((cs(1) \ "print_jobs").as[Seq[JsObject]]).size == 10)
    val j = (cs.head \ "print_jobs").as[Seq[JsObject]].head
    assert(j.keys == Set("id", "team", "computer", "area", "printed", "error", "pages", "arrived"))
    assert((r.json \ "printers").as[Seq[JsObject]].map(p => (p \ "condition").as[String]) == Seq("paper out"))
    val asked = spy.asked.toArray.toSeq.asInstanceOf[Seq[(Seq[Int], Option[Int], Option[String], Int)]]
    assert(asked.forall(_._1.size == 1) && asked.map(_._1.head).sorted == Seq(1, 2))
    assert(!asked.exists(_._1.contains(11)))
    assert(asked.forall(_._4 == 10))
  }

  test("/printing contests param limits, and explicit contest 11 is not allowed") {
    val spy = new Spy(fake())
    val r = printing(spy, "audience" -> "all", "contests" -> "11")
    assert(!spy.asked.toArray.toSeq.toString.contains("11"), spy.asked.toString)
    assert((r.json \ "printers").as[Seq[JsObject]].size == 1, r.body)
  }

  test("/printing team and area filters go to the source; team=x is 400") {
    val spy = new Spy(fake())
    val r = printing(spy, jury ++ Seq("team" -> "6", "area" -> "linux room"): _*)
    assert(r.status == 200, r.body)
    val cs = (r.json \ "contests").as[Seq[JsObject]]
    assert(cs.flatMap(c => (c \ "print_jobs").as[Seq[JsObject]]).map(j => (j \ "id").as[Int]) == Seq(2))
    assert(spy.asked.toArray.toSeq.toString.contains("Some(6)"))
    assert(printing(fake(), jury :+ ("team" -> "x"): _*).status == 400)
  }

  test("/printing with no contest still lists printers") {
    val src = new FakeSource(contestList = Nil, printers = problemsList)
    val r = printing(src, jury: _*)
    assert(r.status == 200, r.body)
    assert((r.json \ "contests").as[Seq[JsObject]].isEmpty)
    assert((r.json \ "note").as[String] == "no contest")
    assert((r.json \ "printers").as[Seq[JsObject]].size == 1)
  }

  test("/printing requires contests") {
    assert(printing(fake(), "audience" -> "all").status == 400)
  }

  test("/printing source failure is a 500 without detail") {
    val bad = new Spy(fake()) {
      override def printJobs(cs: Seq[Int], t: Option[Int], a: Option[String], l: Int) =
        Future.failed(new RuntimeException("secret detail"))
    }
    val r = printing(bad, jury: _*)
    assert(r.status == 500 && !r.body.contains("secret"), r.body)
  }
}
