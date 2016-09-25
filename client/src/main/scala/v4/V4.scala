package v4

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.querki.jquery._

import scala.scalajs.js.annotation.JSExport

object V4 extends js.JSApp {
  import scala.concurrent.ExecutionContext.Implicits.global

  override def main(): Unit = {}

  @JSExport
  def ackWaiterTask(target: String, btnid: String): Unit = {
    $("#wa-" + btnid).html("...")
    Ajax.post(target).onSuccess {
      case xhr =>
        $("#wa-" + btnid).html("<span class=\"caret\"></span>")
    }
  }
}