package models

trait SubmitLike

trait SubmitStore {
  def byID(id: Int): Option[FullyDescribedSubmit]
  def forContest(contestId: Int): Seq[FullyDescribedSubmit]
  def forTeam(contestId: Int, teamLocalId: Int): Seq[FullyDescribedSubmit]
}