package org.gregrice.scores

import scala.io.{BufferedSource, Source}
import scala.language.postfixOps

case class Team(name: String) {
  override def toString: String = this.name
}

case class GoalRecord(team: Team, goals: Int) // For an individual game, goals
object GoalRecord {
  def apply(teamString: String, goals: Int): GoalRecord = GoalRecord(Team(teamString), goals)
}

case class PointRecord(team: Team, points: Int) {
  override def toString: String = s"($team, $points)"
}

object PointRecord {
  def apply(teamString: String, points: Int): PointRecord = PointRecord(Team(teamString), points)
}

case class RankedRecord(rank: Int, pointRecord: PointRecord) {
  override def toString: String = {
    val pointsString = if (pointRecord.points == 1) "pt" else "pts"
    s"$rank. ${pointRecord.team}, ${pointRecord.points} $pointsString"
  }
}

case class Game(teamOneGoals: GoalRecord, teamTwoGoals: GoalRecord) {
  // Define winner and loser, or None if draw
  def winnerAndLoser: Option[(Team, Team)] = {
    if (teamOneGoals.goals > teamTwoGoals.goals) Some((teamOneGoals.team, teamTwoGoals.team))
    else if (teamTwoGoals.goals > teamOneGoals.goals) Some((teamTwoGoals.team, teamOneGoals.team))
    else None
  }

  def teamOne: Team = teamOneGoals.team

  def teamTwo: Team = teamTwoGoals.team
}

object Game {
  // Mostly for ease of testing. I like using case classes for the type safety normally.
  def apply(teamOne: String, teamOneScore: Int, teamTwo: String, teamTwoScore: Int): Game =
    Game(GoalRecord(teamOne, teamOneScore), GoalRecord(teamTwo, teamTwoScore))
}

class ScoresTable(val games: Seq[Game]) {

  def pointRecords: List[PointRecord] = (games.flatMap { game =>
    game.winnerAndLoser match {
      case Some((winner, loser)) =>
        Seq(PointRecord(winner, 3), PointRecord(loser, 0))
      case None =>
        Seq(PointRecord(game.teamOne, 1), PointRecord(game.teamTwo, 1))
    }
  } groupBy (_.team) map { case (team, scores) =>
    (team, scores.map(_.points).reduce(_ + _))
  } toList) sortBy (-_._2) map { case (team, totalPoints) => PointRecord(team, totalPoints) }

  def pointRecordsWithRank: List[RankedRecord] = pointRecords.zipWithIndex.foldLeft(List.empty[(Int, RankedRecord)]) { (results, indexedRecord) =>
    val idx = indexedRecord._2
    val rank = results.lastOption match {
      case Some((lastIdx, RankedRecord(lastRank, lastRecord))) => if (lastRecord.points == indexedRecord._1.points) {
        lastRank
      } else
        lastIdx + 2
      case None => idx + 1
    }
    results.appended(idx, RankedRecord(rank, indexedRecord._1))
  }.map(_._2)


}

object ScoresTable extends App {

  private[scores] def parseLine(line: String): Game = {
    line.split(", ").toList
      .map { teamLine =>
        val teamNameLine :+ goals = teamLine.split(" ").toList
        val team = teamNameLine.mkString(" ")
        GoalRecord(team, goals.toInt)
      } match {
      case List(recordTeamOne, recordTeamTwo) => Game(recordTeamOne, recordTeamTwo)
    }
  }

  private val reader: BufferedSource = args.headOption match {
    case Some(file) =>
      Source.fromFile(file)
    case None =>
      Source.stdin
  }

  private[scores] def parseGames(gameLines: Seq[String]) = gameLines.map(parseLine)

  try {
    val scoresTable: ScoresTable = new ScoresTable(parseGames(reader.getLines.toSeq))
    scoresTable.pointRecordsWithRank.foreach(println)
  } finally {
    reader.close()
  }
}
