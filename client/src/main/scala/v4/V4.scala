package v4

import scala.scalajs.js
import org.scalajs.dom

object ScalaJSExample extends js.JSApp {
  def main(): Unit = {
    dom.document.getElementById("scalajsShoutOut").textContent = "test"
  }
}