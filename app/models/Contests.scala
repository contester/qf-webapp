package models

import com.github.nscala_time.time.Imports._

case class Contest(id: Int, name: String, schoolMode: Boolean, startTime: DateTime, endTime: DateTime,
                   freezeTime: DateTime, exposeTime: DateTime) {
  def frozen = (DateTime.now >= freezeTime) && (DateTime.now < exposeTime)
  def finished = DateTime.now >= endTime
  def started = DateTime.now >= startTime

}

case class Problem(id: String, name: String, tests: Int, rating: Int)