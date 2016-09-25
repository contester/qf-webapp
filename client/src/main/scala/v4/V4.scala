package v4

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.querki.jquery._
import org.scalajs.dom.experimental.Notification

import scala.concurrent.Future
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

  def btn0(target: String, btnid: String): Future[JQuery] = {
    val b = $("#" + btnid)
    b.html("...")
    val f =  Ajax.post(target)
    f.onFailure {
      case e => dom.console.error(e.toString)
        b.html("!")
    }
    f.map { x =>
      b.html("<span class=\"caret\"></span>")
      b
    }
  }

  @JSExport
  def btn(target: String, btnid: String): Unit = {
    btn0(target, btnid)
  }

  @JSExport
  def btnDelete(target: String, btnid: String, rowid: String) {
    btn0(target, btnid).onSuccess {
      case x => $(s"#$rowid").remove()
    }
  }
}