package org.stingray.qf.models.monitor

import spire.math.Rational

import scala.collection.immutable.SortedSet

trait Submit extends Ordered[Submit] {
  def id: Int
  def fraction: Rational
}

trait SubmitIndex {
  def withSubmit(s: Submit): SubmitIndex
  def withoutSubmit(s: Int): SubmitIndex
  def submits: Seq[Submit]
}

case class StaticSubmitIndex(submits: Seq[Submit]) extends SubmitIndex {
  override def withSubmit(s: Submit): SubmitIndex = {
    StaticSubmitIndex(submits.filterNot(_.id == s.id).+:(s).sorted)
  }

  override def withoutSubmit(s: Int) =
    StaticSubmitIndex(submits.filterNot(_.id == s))
}

trait AbstractCell

trait Cell[ScoreType <: AbstractScore] extends AbstractCell {
  def score: ScoreType

  def withSubmit(s: Submit): Cell[ScoreType]
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

object SchoolCell {
  def scoreSubmits(base: Int, submits: Seq[Submit]):SchoolScore =
    SchoolScore(submits.foldLeft((base, Rational.zero)) {
    case (prev, sub) =>
      val nextBase = if (prev._1 <= 20) 20 else prev._1-1
      val nextScore: Rational = if (sub.fraction == 1) nextBase else nextBase * 2 * sub.fraction / 3
      (nextBase, if (nextScore > 5) nextScore else 0)
  }._2)
}

case class SchoolCell(base: Int, submits: SubmitIndex, score: SchoolScore) extends Cell[SchoolScore] {
  import SchoolCell._
  override def withSubmit(s: Submit): Cell[SchoolScore] = {
    val next = submits.withSubmit(s)
    SchoolCell(base, next, scoreSubmits(base, next.submits))
  }

  override def withoutSubmit(id: Int): Cell[SchoolScore] = {
    val next = submits.withoutSubmit(id)
    SchoolCell(base, next, scoreSubmits(base, next.submits))
  }
}