package v4

import org.querki.jquery._
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.experimental.Notification
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.EventSource

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

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

  @JSExport
  def listen(target: String, setup: EventV2.SetupFunction, state: EventV2.StateFunction): Unit = {
    val source = new EventSource(target)

    var pingState: Option[Int] = None

    def reconnect(): Unit = {
      source.close()
      dom.window.setTimeout(() => listen(target, setup, state), 1000)
    }

    def clearPingState(): Unit = {
      for (s <- pingState) {
        dom.window.clearTimeout(s)
        pingState = None
      }
    }

    def resetPingState(): Unit = {
      clearPingState()
      pingState = Some(dom.window.setTimeout(() => reconnect(), 60 * 1000))
    }

    source.onopen = (e: Event) => {
      state(true)
      resetPingState()
    }

    source.onerror = (e: Event) => {
      state(false)
      clearPingState()
      if (source.readyState == 2) {
        reconnect()
      }
    }

    source.addEventListener("ping", (_: Event) => resetPingState())

  }
}

object EventV2 {
  type JsonHandler = (js.Dynamic) => Unit
  type AddEventListener = (String, JsonHandler) => Unit
  type SetupFunction = (AddEventListener) => Unit
  type StateFunction = (Boolean) => Unit
}