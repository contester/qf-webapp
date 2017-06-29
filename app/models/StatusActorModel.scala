package models

import javax.inject.{Inject, Singleton}

import actors.{StatusActor, WaiterActor}
import akka.actor.ActorSystem
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

@Singleton
class StatusActorModel @Inject() (dbConfigProvider: DatabaseConfigProvider, system: ActorSystem) {
  private val db = dbConfigProvider.get[JdbcProfile].db
  val statusActor = system.actorOf(StatusActor.props(db), "status-actor")
  val waiterActor = system.actorOf(WaiterActor.props(db), "waiter-actor")
}