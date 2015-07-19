package controllers

import javax.inject.{Inject, Singleton}

import models.Users
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.jdbc.JdbcBackend

import scala.concurrent.{Future, ExecutionContext}

@Singleton
class AuthWrapper @Inject() (dbConfigProvider: DatabaseConfigProvider) {
  val db = dbConfigProvider.get[JdbcProfile].db

  def resolve(username: String)(implicit ctx: ExecutionContext) =
    db.run(Users.resolveQuery(username)).map(_.headOption)
}