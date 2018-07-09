package utils.auth

import com.mohiva.play.silhouette.api.Env
import com.mohiva.play.silhouette.impl.authenticators.{CookieAuthenticator, SessionAuthenticator}
import models.{Admin, LoggedInTeam}

trait TeamsEnv extends Env {
  type I = LoggedInTeam
  type A = SessionAuthenticator
}

trait AdminEnv extends Env {
  type I = Admin
  type A = SessionAuthenticator
}