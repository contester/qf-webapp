package controllers

import javax.inject.{Inject, Singleton}

import org.webjars.play.RequireJS
import play.api.mvc.{Action, Controller}

@Singleton
class RjsHack @Inject() (requireJS: RequireJS) extends Controller  {
  def index =
    Action {
      Ok(requireJS.setup(routes.WebJarAssets.at("")))
    }

}