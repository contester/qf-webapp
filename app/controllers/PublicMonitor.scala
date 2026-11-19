package controllers

import models.{Anonymous, BoardSource, PublicBoardCache}
import org.joda.time.DateTime
import play.api.i18n.Messages
import play.api.mvc._
import views.html

import scala.concurrent.Future

/**
  * The unauthenticated scoreboard: 403 while the round runs (404 for an unknown round), then the final points board,
  * rendered on demand from the database and kept in `cache` until a notification changes it. There is no public live
  * stream. No session or cookie is read or created here.
  */
class PublicMonitor(cc: ControllerComponents, boards: BoardSource, cache: PublicBoardCache) extends AbstractController(cc) {
  private implicit val ec = defaultExecutionContext

  def page(contestId: Int): Action[AnyContent] = pageAt(contestId, () => DateTime.now)

  private[controllers] def pageAt(contestId: Int, now: () => DateTime): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = messagesApi.preferred(request.acceptLanguages) // no cookie is read here
    // The round is read afresh each time, so the check below sees an early finish or a reopened round at once.
    boards.round(contestId).flatMap { round =>
      BoardAccess.decide(round, Anonymous, now()) match {
        case Left(result) => Future.successful(result)
        case Right(contest) =>
          cache.getOrRender(contest.id, messages.lang.code) {
            boards.board(contest.id).map(b => html.staticmonitor(contest, b).body)
          }.map(page => Ok(page).as(HTML))
      }
    }
  }
}
