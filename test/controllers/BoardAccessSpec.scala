package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.testkit.TestKit
import com.typesafe.config.ConfigFactory
import models._
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.Contest
import play.api.i18n.{DefaultLangs, DefaultMessagesApi, Lang, MessagesImpl}
import play.api.mvc._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source

/**
  * The page-level visibility of the scoreboard: the pure decision every monitor page uses (BoardAccess.decide).
  * Only admins view the scoreboard in qf-webapp; contestant and public scoreboards are in teacher-web.
  */
class BoardAccessSpec extends TestKit(ActorSystem("BoardAccessSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val mat: Materializer = Materializer(system)

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

  test("decide: teams and the public always get 403 while admins always get the round") {
    for (rd <- Seq(round(), round(finished = true)); now <- Seq(running, end, end.plusHours(1))) {
      for (v <- Seq(TeamViewer(1, 5), Anonymous)) {
        val r = BoardAccess.decide(Some(rd), v, now)
        assert(r.isLeft)
        val res = r.left.get
        assert(res.header.status == 403)
        assert(body(res) == notPublished)
      }
      assert(BoardAccess.decide(Some(rd), AdminViewer, now) == Right(rd))
    }
  }
}
