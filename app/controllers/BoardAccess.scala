package controllers

import models.{Viewer, Visibility}
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest
import play.api.i18n.Messages
import play.api.mvc.{Result, Results}

/** The page-level visibility of the scoreboard, shared by the team, public and admin monitor pages. */
object BoardAccess {
  /**
    * The round to show the board of, or the response instead: 404 for an unknown round, 403 with
    * monitor.notPublished while the viewer may not see the board yet (see Visibility.canSeeBoard).
    */
  def decide(round: Option[Contest], viewer: Viewer, now: DateTime)(implicit messages: Messages): Either[Result, Contest] =
    round match {
      case None => Left(Results.NotFound)
      case Some(r) if Visibility.canSeeBoard(r, viewer, now) => Right(r)
      case Some(_) => Left(Results.Forbidden(messages("monitor.notPublished")))
    }
}
