package palevo.query

import scala.util.Try

sealed trait Audience
/** `audience=public`: what the public may see (the freezing board). */
case object Public extends Audience
/** `audience=all`: the jury's view (the real board, jury-only fields). */
case object Jury extends Audience

/** The scope palevo sends with every request. `contests = None` means `*` (every allowed contest). */
case class Scope(audience: Audience, contests: Option[Set[Int]])

object Scope {
  private val Id = "[0-9]+".r

  /** Left is a short English error text for a 400. */
  def parse(audience: Option[String], contests: Option[String]): Either[String, Scope] =
    for {
      a <- audience match {
        case Some("public") => Right(Public)
        case Some("all") => Right(Jury)
        case Some(_) => Left("audience must be public or all")
        case None => Left("missing audience")
      }
      c <- contests match {
        case None => Left("missing contests")
        case Some("*") => Right(None)
        case Some(s) =>
          val ids = s.split(",", -1).toSeq.map {
            case p @ Id() => Try(p.toInt).toOption
            case _ => None
          }
          if (ids.nonEmpty && ids.forall(_.isDefined)) Right(Some(ids.flatten.toSet))
          else Left("contests must be * or a comma list of ids")
      }
    } yield Scope(a, c)
}
