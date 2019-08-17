package models

import actors.{StatusActor, WaiterActor}
import akka.actor.ActorSystem
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

class StatusActorModel (dbConfig: DatabaseConfig[JdbcProfile], system: ActorSystem) {
  private val db = dbConfig.db
  val statusActor = system.actorOf(StatusActor.props(db), "status-actor")
  val waiterActor = system.actorOf(WaiterActor.props(db), "waiter-actor")
}