package palevo

import play.api.{Configuration, Logger}

import scala.util.{Failure, Success, Try}

/**
  * Minutes before the start and before the end of a contest at which palevo is told "in N minutes". An empty list
  * disables that countdown.
  */
final case class CountdownConfig(start: Seq[Int], end: Seq[Int])

object CountdownConfig {
  private val DefaultStart = Seq(60, 30, 20, 10, 5, 2, 1)
  private val DefaultEnd = Seq(20, 10, 5, 1)

  val Default: CountdownConfig = CountdownConfig(DefaultStart, DefaultEnd)

  private val log = Logger(getClass)

  /** `xs` sorted from the largest and without repeats; `default` if any entry is not positive (a typo must not stop the app). */
  def sanitize(xs: Seq[Int], default: Seq[Int]): Seq[Int] =
    if (xs.exists(_ <= 0)) default else xs.distinct.sorted(Ordering[Int].reverse)

  /** Reads `palevo.countdown.start` and `palevo.countdown.end` (lists of minutes); a missing key means the default. */
  def apply(cfg: Configuration): CountdownConfig = {
    def read(key: String, default: Seq[Int]): Seq[Int] =
      Try(cfg.getOptional[Seq[Int]](key)) match {
        case Success(xs) => xs.fold(default) { xs =>
          if (xs.exists(_ <= 0)) log.warn(s"$key must list positive minutes; using the default ${default.mkString(",")}")
          sanitize(xs, default)
        }
        case Failure(e) =>
          log.warn(s"$key must be a list of minutes (${e.getMessage}); using the default ${default.mkString(",")}")
          default
      }
    CountdownConfig(read("palevo.countdown.start", DefaultStart), read("palevo.countdown.end", DefaultEnd))
  }
}
