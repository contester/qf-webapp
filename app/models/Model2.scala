package org.stingray.qf.models

import models.Team

case class TeamSchool(id: Int, name: String)

case class GlobalTeamState(id: Int, school: TeamSchool, num: Int, name: String)

case class LocalTeamState(contest: Int, team: GlobalTeamState, id: Int,
                          disabled: Boolean, noPrint: Boolean, notRated: Boolean) extends Team {
  override def schoolName: String = team.school.name

  override def teamNum: Option[Int] = if (team.num != 0) Some(team.num) else None

  override def teamName: String = team.name
}
