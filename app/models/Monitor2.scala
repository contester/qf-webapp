package org.stingray.qf.models.monitor

import cats.kernel.Monoid
import models.{Contest, Problem, Submit, Team}
import spire.math.Rational

import scala.collection.immutable.SortedSet

trait CellSubmit extends Ordered[CellSubmit] {
  def id: Int
  def arrivedSeconds: Int
  def fraction: Rational
  def passed: Int
  def taken: Int
}

case class StaticCellSubmit(s: Submit) extends CellSubmit {
  override def id: Int = s.submitId.id

  override def arrivedSeconds: Int = s.submitId.arrived.seconds

  override def fraction: Rational = s.passed / s.taken

  override def passed: Int = s.passed

  override def taken: Int = s.taken

  override def compare(that: CellSubmit): Int = arrivedSeconds.compareTo(that.arrivedSeconds)
}

trait SubmitIndex {
  def withSubmit(s: CellSubmit): SubmitIndex
  def withoutSubmit(s: Int): SubmitIndex
  def submits: Seq[CellSubmit]
}

case class StaticSubmitIndex(submits: Seq[CellSubmit]) extends SubmitIndex {
  override def withSubmit(s: CellSubmit): SubmitIndex = {
    StaticSubmitIndex(submits.filterNot(_.id == s.id).+:(s).sorted)
  }

  override def withoutSubmit(s: Int) =
    StaticSubmitIndex(submits.filterNot(_.id == s))
}

trait AbstractCell

trait Cell[ScoreType <: AbstractScore] extends AbstractCell {
  def score: ScoreType

  def withSubmit(s: CellSubmit): Cell[ScoreType]
  def withoutSubmit(id: Int): Cell[ScoreType]
}

trait AbstractScore

trait AbstractRow {
  def score: AbstractScore
  def cells: Map[String, AbstractCell]
}

case class ConcreteRow[ScoreType <: AbstractScore](team: Int, score: ScoreType, cells: Map[String, Cell[ScoreType]]) extends AbstractRow

case class FullMonitor[ScoreType <: AbstractScore](rows: Seq[(ConcreteRow[ScoreType], Option[Int])])

case class SchoolScore(val r: Rational) extends AbstractScore

object SchoolScore {
  implicit val schoolScoreMonoid = new Monoid[SchoolScore] {
    override def empty: SchoolScore = SchoolScore(0)

    override def combine(x: SchoolScore, y: SchoolScore): SchoolScore =
      SchoolScore(x.r + y.r)
  }
}

object SchoolCell {
  def scoreSubmits(base: Int, submits: Seq[CellSubmit]):SchoolScore =
    SchoolScore(submits.foldLeft((base, Rational.zero)) {
    case (prev, sub) =>
      val nextBase = if (prev._1 <= 20) 20 else prev._1-1
      val nextScore: Rational = if (sub.fraction == 1) nextBase else nextBase * 2 * sub.fraction / 3
      (nextBase, if (nextScore > 5) nextScore else 0)
  }._2)

  def apply(submits: Seq[CellSubmit]): SchoolCell = {
    val index = StaticSubmitIndex(submits)
    SchoolCell(30, index, SchoolCell.scoreSubmits(30, index.submits))
  }
}

case class SchoolCell(base: Int, submits: SubmitIndex, score: SchoolScore) extends Cell[SchoolScore] {
  import SchoolCell._
  override def withSubmit(s: CellSubmit): Cell[SchoolScore] = {
    val next = submits.withSubmit(s)
    SchoolCell(base, next, scoreSubmits(base, next.submits))
  }

  override def withoutSubmit(id: Int): Cell[SchoolScore] = {
    val next = submits.withoutSubmit(id)
    SchoolCell(base, next, scoreSubmits(base, next.submits))
  }
}

case class MonitorSourceData(contest: Contest, problems: Map[String, Problem], teams: Map[Int, Team], submits: Seq[Submit])

object MonitorBuilder {
  def filterSubmits(data: MonitorSourceData) =
    data.submits.filter { x =>
      x.finished && x.compiled && !x.afterFreeze && data.problems.isDefinedAt(x.submitId.problem.id) &&
      data.teams.isDefinedAt(x.submitId.teamId)
    }

  implicit val schoolOrder = Ordering.by { foo: SchoolScore =>
    foo.r
  }

  def buildRows[ScoreType <: AbstractScore](data: MonitorSourceData, empty: ScoreType): Seq[ConcreteRow[SchoolScore]] = {
    filterSubmits(data).groupBy(_.submitId.teamId).mapValues { perTeam =>
      perTeam.groupBy(_.submitId.problem.id).mapValues { perCell =>
        val index = StaticSubmitIndex(perCell.map(StaticCellSubmit))
        SchoolCell(30, index, SchoolCell.scoreSubmits(30, index.submits))
      }
    }.map {
      case (teamId, cells) =>
        val sc = cells.values.foldLeft(Rational(0))((a, b) => a + b.score.r)
        ConcreteRow(teamId, SchoolScore(sc), cells)
    }.toSeq.sortBy(_.score).reverse
  }
}