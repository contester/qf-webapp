package actors

import scala.collection.mutable

/**
  * Keeps a rescan of the contests table from undoing an edit that reached the actor while the rescan was running:
  * a rescan answers with the number it started under, and is not believed about a contest that was edited after
  * that. Single-threaded: one actor owns each instance.
  */
final class ContestRowGuard {
  private var generation = 0L
  private val changedAt = mutable.Map.empty[Int, Long]

  /** A rescan is starting; give its answer back to `believes`. */
  def beginRescan(): Long = {
    generation += 1
    generation
  }

  /** The application edited contest `id`; its new row is already known. */
  def changed(id: Int): Unit = {
    generation += 1
    changedAt(id) = generation
  }

  /** Whether the row of `id` read by the rescan that started as `startedAt` may be used. */
  def believes(id: Int, startedAt: Long): Boolean = changedAt.get(id).forall(_ <= startedAt)
}
