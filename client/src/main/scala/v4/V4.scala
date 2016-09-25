package v4

import scala.scalajs.js
import org.scalajs.dom

import scala.scalajs.js.annotation.JSExport

object ScalaJSExample extends js.JSApp {
  def main(): Unit = {
    dom.document.getElementById("scalajsShoutOut").textContent = "test"
  }

  @JSExport
  def clickTest(s: String): Unit = {
    dom.window.alert(s)
  }
}