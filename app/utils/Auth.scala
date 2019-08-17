package utils.auth

import com.mohiva.play.silhouette.api.Env
import com.mohiva.play.silhouette.impl.authenticators.{CookieAuthenticator, SessionAuthenticator}
import models.{Admin, LoggedInTeam}

trait BaseEnv extends Env {
  type A = SessionAuthenticator
}

trait TeamsEnv extends BaseEnv {
  type I = LoggedInTeam
}

trait AdminEnv extends BaseEnv {
  type I = Admin
}