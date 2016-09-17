package models

import javax.inject.{Inject, Singleton}

import actors.{StatusActor, WaiterActor}
import akka.actor.ActorSystem
import play.api.db.slick.DatabaseConfigProvider
import play.api.mvc.RequestHeader
import slick.driver.JdbcProfile

import scala.concurrent.ExecutionContext

object AllActors {
  val standardTimeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }
}

import akka.pattern.ask

@Singleton
class StatusActorModel @Inject() (dbConfigProvider: DatabaseConfigProvider, system: ActorSystem) {
  val statusActor = system.actorOf(StatusActor.props(dbConfigProvider.get[JdbcProfile].db), "status-actor")
}

@Singleton
class WaiterActorModel @Inject() (dbConfigProvider: DatabaseConfigProvider, system: ActorSystem) {
  val waiterActor = system.actorOf(WaiterActor.props(dbConfigProvider.get[JdbcProfile].db), "waiter-actor")

  def getSnapshot(rooms: List[String])(implicit ec: ExecutionContext) =
    waiterActor.ask(WaiterActor.GetSnapshot(rooms))(AllActors.standardTimeout).mapTo[WaiterActor.Snapshot].map(_.tasks)

  def join(rooms: List[String], requestHeader: RequestHeader)(implicit ec: ExecutionContext) =
    waiterActor.ask(WaiterActor.Join(rooms, requestHeader))(AllActors.standardTimeout).mapTo[WaiterActor.Joined].map(_.enum)
}