package org.stingray.qf.models.monitor

trait Submit {
  def id: Int
}

trait AbstractCell

trait Cell[ScoreType <: AbstractScore] extends AbstractCell {
  def withSubmit(s: Submit): Cell[ScoreType]
  def withoutSubmit(id: Int): Cell[ScoreType]
}

trait AbstractScore

trait AbstractRow {
  def score: AbstractScore
  def cells: Map[String, AbstractCell]
}

case class ConcreteRow[ScoreType <: AbstractScore](team: Int, score: ScoreType, cells: Map[String, Cell[ScoreType]]) extends AbstractRow