package modules

import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport

import actors.StatusActor

class StatusModule extends AbstractModule with AkkaGuiceSupport {
  def configure = {
    bindActor[StatusActor]("status-actor")
  }
}

