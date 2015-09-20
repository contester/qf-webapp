package models

import javax.inject.{Inject, Singleton}

import actors.StatusActor
import akka.actor.ActorSystem
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile

@Singleton
class StatusActorModel @Inject() (dbConfigProvider: DatabaseConfigProvider, system: ActorSystem) {
  val statusActor = system.actorOf(StatusActor.props(dbConfigProvider.get[JdbcProfile].db), "status-actor")
}