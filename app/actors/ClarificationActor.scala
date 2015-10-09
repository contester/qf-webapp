package actors

import akka.actor.{ActorRef, Stash, Props, Actor}
import models.{ContestTeamIds, Clarification}
import play.api.libs.EventSource
import play.api.libs.EventSource.{Event, EventDataExtractor, EventNameExtractor}
import play.api.libs.iteratee.{Enumerator, Concurrent, Enumeratee}
import play.api.libs.json.Json
import slick.jdbc.{GetResult, JdbcBackend}

import scala.collection.mutable
import scala.concurrent.ExecutionContext

import com.github.nscala_time.time.Imports._

case class MaxSeen(contest: Int, team: Int, timestamp: DateTime)

object MaxSeen {
  implicit val getResult = GetResult(r =>
    MaxSeen(r.nextInt(), r.nextInt(), new DateTime(r.nextTimestamp()))
  )
}