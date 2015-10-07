package models

import play.api.libs.concurrent.Akka

import scala.concurrent.ExecutionContext

object Contexts {
  import play.api.Play.current
  implicit val gridfsExecutionContext: ExecutionContext = Akka.system.dispatchers.lookup("gridfs-context")
  implicit val adminExecutionContext: ExecutionContext = Akka.system.dispatchers.lookup("admin-context")
}