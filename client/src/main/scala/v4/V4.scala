package v4

import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.querki.jquery.{$, JQuery}

@js.native
trait Material extends js.Object {
  def init(): Unit = js.native
}

@js.native
trait JQueryMaterial extends JQuery {
  def material: Material = js.native
}

object JQueryMaterial {
  implicit def jq2material(jq: JQuery): JQueryMaterial =
    jq.asInstanceOf[JQueryMaterial]
}

object V4 extends js.JSApp {
  override def main(): Unit = {
    $( () => {
      println("blablabla")

      import JQueryMaterial._

      $().material.init()

      println("ghr")
    })
  }
}

@JSExportTopLevel("QFWeb")
object QFWeb {
  @JSExport
  def listenAdmin(path: String, iconbase: String) = {
    println(s"$path - $iconbase")
  }
}