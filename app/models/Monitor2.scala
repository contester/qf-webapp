package org.stingray.qf.models.monitor

import spire.math.Rational

trait Submit {
  def id: Int
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

case class SchoolScore(val r: Rational) extends AbstractScore

case class SchoolCell(base: Int, submits: Seq[Submit], score: SchoolScore) extends Cell[SchoolScore] {
  override def withSubmit(s: Submit): Cell[SchoolScore] = {
    ???
  }

  override def withoutSubmit(id: Int): Cell[SchoolScore] = ???
}