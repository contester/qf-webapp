package controllers

import javax.inject.Singleton

import play.api.mvc.{Action, Controller}

@Singleton
class RjsHack extends Controller  {
  def index =
    Action {
      Ok(org.webjars.RequireJS.getSetupJavaScript(routes.WebJarAssets.at("").url))
    }
}