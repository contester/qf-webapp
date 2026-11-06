package models

import protos.Tickets.PrinterAlert

/** Which printer problems people were told about, so that a repeat is not announced again. Kept in memory: after a
  * restart a problem that is still there is announced once more by the agent, and one that went away meanwhile
  * just never gets its "working again". */
class PrinterAlertState {
  private val open = scala.collection.mutable.Map.empty[(String, String), String]

  /** Whether this alert is news. */
  def isNews(a: PrinterAlert): Boolean = synchronized {
    val key = (a.agent, a.printer)
    if (a.open) {
      val news = !open.get(key).contains(a.condition)
      open(key) = a.condition
      news
    } else open.remove(key).isDefined
  }

  /** The problems announced and not yet cleared, as (agent, printer, condition). */
  def currentProblems: Seq[(String, String, String)] = synchronized {
    open.toList.map { case ((agent, printer), condition) => (agent, printer, condition) }.sorted
  }
}
