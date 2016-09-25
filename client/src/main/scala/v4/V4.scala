package v4

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.querki.jquery._
import org.scalajs.dom.experimental.Notification

import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success}

object V4 extends js.JSApp {
  import scala.concurrent.ExecutionContext.Implicits.global

  override def main(): Unit = {}

  @JSExport
  def askForPermission(): Unit = {
    if (Notification.permission != "granted")
      Notification.requestPermission { x: String =>
        ()
      }
  }

  @JSExport
  def ackWaiterTask(target: String, btnid: String): Unit = {
    $("#wa-" + btnid).html("...")
    Ajax.post(target).onSuccess {
      case xhr =>
        $("#wa-" + btnid).html("<span class=\"caret\"></span>")
    }
  }

  @JSExport
  def btn(target: String, btnid: String): Unit = {
    val b = $("#" + btnid)
    b.html("...")
    Ajax.post(target).onComplete {
      case Success(xhr) =>
        b.html("<span class=\"caret\"></span>")
      case Failure(e) =>
        dom.console.error(e.toString)
        b.html("!")
    }
  }
}