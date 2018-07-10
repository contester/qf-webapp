package controllers

import java.security.MessageDigest
import javax.inject.{Inject, Singleton}

import models.{AdminId, Admin, Users}
import play.api.Configuration
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext

object Hasher {
  private val hasher = MessageDigest.getInstance("SHA-1")

  private def bytesToString(x: Array[Byte]) = x.map("%02X" format _).mkString

  def getSha1(x: Array[Byte]): String =
    bytesToString(hasher.digest(x)).toLowerCase

  def getSha1(x: String): String =
    getSha1(x.getBytes)
}