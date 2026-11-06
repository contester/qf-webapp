package palevo.query

import play.api.Configuration

/** Configuration of the read-only query API for palevo. No token (or a blank one) disables the API: every endpoint
 * answers 404. `allowedContests` caps every scope: contest ids outside it are never served. */
case class QueryConfig(token: Option[String], allowedContests: Set[Int])

object QueryConfig {
  val defaultContests: Set[Int] = (1 to 10).toSet

  def apply(c: Configuration): QueryConfig =
    QueryConfig(
      token = c.getOptional[String]("palevo.query.token").map(_.trim).filter(_.nonEmpty),
      allowedContests = c.getOptional[Seq[Int]]("palevo.query.contests").map(_.toSet).getOrElse(defaultContests))
}
