package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import akka.testkit.TestKit
import com.typesafe.config.ConfigFactory
import live.{LiveMonitorConfig, LiveMonitorRef}
import models._
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{Contest, Team}
import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.i18n.{DefaultLangs, DefaultMessagesApi, Lang, MessagesImpl}
import play.api.libs.typedmap.TypedMap
import play.api.mvc._
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

/**
  * The page-level visibility of the scoreboard: the pure decision every monitor page uses (BoardAccess.decide), and
  * the public page driven end to end through PublicMonitor.page with a fake board source. The team (/monitor) and
  * admin (/admin/monitor/:id) pages need Silhouette to drive; they call the same decision with TeamViewer and
  * AdminViewer and a round re-read from the database.
  */
class BoardAccessSpec extends TestKit(ActorSystem("BoardAccessSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val mat: Materializer = Materializer(system)
  private implicit val ec: ExecutionContext = system.dispatcher

  private val ru: Map[String, String] = {
    val s = Source.fromFile("conf/messages.ru", "UTF-8")
    try s.getLines().map(_.trim).filter(l => l.contains("=") && !l.startsWith("#"))
      .map { l => val i = l.indexOf('='); l.substring(0, i).trim -> l.substring(i + 1).trim }.toMap
    finally s.close()
  }
  private val messagesApi = new DefaultMessagesApi(Map("ru" -> ru), new DefaultLangs(Seq(Lang("ru"))))
  private implicit val messages: MessagesImpl = MessagesImpl(Lang("ru"), messagesApi)

  private val notPublished = "Результаты будут опубликованы после окончания тура"

  private val start = new DateTime(2030, 1, 1, 10, 0)
  private val end = start.plusHours(3)
  private def round(finished: Boolean = false) = Contest(5, "R", start, end, None, 1, isFinished = finished)
  private val running = start.plusHours(1)

  private def body(r: Result): String = Await.result(r.body.consumeData, 5.seconds).utf8String

  test("the message key has the specified Russian text, and an English one") {
    assert(ru.get("monitor.notPublished").contains(notPublished))
    val en = scala.io.Source.fromFile("conf/messages", "UTF-8")
    try assert(en.getLines().exists(_.startsWith("monitor.notPublished=")))
    finally en.close()
  }

  test("decide: an unknown round is 404 for everyone") {
    for (v <- Seq(AdminViewer, TeamViewer(1, 5), Anonymous))
      assert(BoardAccess.decide(None, v, running).left.map(_.header.status) == Left(404))
  }

  test("decide: while the round runs teams and the public get 403 with the message; admins get the round") {
    for (v <- Seq(TeamViewer(1, 5), Anonymous)) {
      val r = BoardAccess.decide(Some(round()), v, running)
      assert(r.isLeft)
      val res = r.left.get
      assert(res.header.status == 403)
      assert(body(res) == notPublished)
    }
    assert(BoardAccess.decide(Some(round()), AdminViewer, running) == Right(round()))
  }

  test("decide: at ends_at, after it, and once finished, everyone gets the round") {
    for (v <- Seq(AdminViewer, TeamViewer(1, 5), Anonymous)) {
      assert(BoardAccess.decide(Some(round()), v, end) == Right(round()))
      assert(BoardAccess.decide(Some(round()), v, end.plusHours(1)) == Right(round()))
      assert(BoardAccess.decide(Some(round(finished = true)), v, running) == Right(round(finished = true)))
    }
  }

  // ---- the public page, driven through the controller ----

  private val cc: ControllerComponents = {
    val parsers = PlayBodyParsers()
    DefaultControllerComponents(DefaultActionBuilder(parsers.defaultBodyParser), parsers, messagesApi,
      new DefaultLangs(Seq(Lang("ru"))), new DefaultFileMimeTypes(FileMimeTypesConfiguration()), ec)
  }

  private val board = MonitorPoints(Seq(ProblemColumn("A", "Alpha", 20)), Seq(
    RankedRow(Some(1), Team(11, 7, "Seven", "Lyceum", "Town", None), Seq(PointsCell(Some(20), 1, full = true)), 20)))

  private def controller(rounds: Map[Int, Contest]) = {
    val asked = new java.util.concurrent.atomic.AtomicInteger(0)
    val source = BoardSource(id => Future.successful(rounds.get(id)), _ => { asked.incrementAndGet(); Future.successful(board) })
    (new PublicMonitor(cc, LiveMonitorConfig(enabled = false, 30.seconds, 60.seconds, Set.empty, 10),
      LiveMonitorRef(None), source, new live.ConnectionStats), asked)
  }

  private def get(path: String): Request[AnyContent] =
    RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget(path, path, Map.empty), "HTTP/1.1", Headers(), TypedMap.empty, AnyContentAsEmpty)

  private def page(rounds: Map[Int, Contest], now: DateTime, id: Int = 5) = {
    val (c, asked) = controller(rounds)
    val r = Await.result(c.pageAt(id, () => now)(get(s"/public/monitor/$id")), 5.seconds)
    (r, asked.get)
  }

  test("public page: 403 with the message while the round runs, without loading the board") {
    val (r, asked) = page(Map(5 -> round()), running)
    assert(r.header.status == 403 && body(r) == notPublished && asked == 0)
  }

  test("public page: 404 for an unknown round") {
    assert(page(Map.empty, running)._1.header.status == 404)
  }

  test("public page: 200 with the board after the end, and when finished early") {
    for ((rd, now) <- Seq(round() -> end, round(finished = true) -> running)) {
      val (r, asked) = page(Map(5 -> rd), now)
      assert(r.header.status == 200 && asked == 1)
      val h = body(r)
      assert(h.contains("Seven") && h.contains("points-cell full-score"))
      assert(r.newSession.isEmpty && r.newCookies.isEmpty)
    }
  }
}
