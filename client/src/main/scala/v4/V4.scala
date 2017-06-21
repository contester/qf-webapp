package v4

import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.querki.jquery.{$, JQuery}
import org.scalajs.dom.experimental.Notification

@js.native
trait Material extends js.Object {
  def init(): Unit = js.native
}

@js.native
trait JQueryMaterial extends JQuery {
  val material: Material = js.native
}

object JQueryMaterial {
  implicit def jq2material(jq: JQuery): JQueryMaterial =
    jq.asInstanceOf[JQueryMaterial]
}

object V4 extends js.JSApp {
  override def main(): Unit = {
    $( () => {
      js.Dynamic.global.$.material.init()
    })
  }
}

@JSExportTopLevel("QFWeb")
object QFWeb {
  @JSExport
  def listenAdmin(path: String, iconbase: String) = {
    println(s"$path - $iconbase")
  }

  @JSExport
  def askForPermission(): Unit = {
    if (Notification.permission != "granted") {
      Notification.requestPermission((_: String) => ())
    }
  }
}