package org.gregrice.scores

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScoresFixture {
  val inputString =
    """Lions 3, Snakes 3
      |Tarantulas 1, FC Awesome 0
      |Lions 1, FC Awesome 1
      |Tarantulas 3, Snakes 1
      |Lions 4, Grouches 0
      |""".stripMargin

  val outputString =
    """
      |
      |""".stripMargin

  val Lions = Team("Lions")
  val Tarantulas = Team("Tarantulas")
  val FcAwesome = Team("FC Awesome")
  val Grouches = Team("Grouches")

  val gamesFixture = Seq(
    Game("Lions", 3, "Snakes", 3),
    Game("Tarantulas", 1, "FC Awesome", 0),
    Game("Lions", 1, "FC Awesome", 1),
    Game("Tarantulas", 3, "Snakes", 1),
    Game("Lions", 4, "Grouches", 0)
  )

  val games = ScoresTable.parseGames(inputString.linesIterator.toSeq)

  val rankedTeamsResultFixture = List(
    (1, PointRecord("Tarantulas", 6)),
    (2, PointRecord("Lions", 5)),
    (3, PointRecord("Snakes", 1)),
    (3, PointRecord("FC Awesome", 1)),
    (5, PointRecord("Grouches", 0))

  )

}

class ScoresSpec extends AnyFlatSpec with Matchers {
  "ScoresTable" should "Parse a game from a line" in new ScoresFixture {
    ScoresTable.parseLine(inputString.linesIterator.toList.head) shouldBe Game(GoalRecord(Team("Lions"), 3), GoalRecord(Team("Snakes"), 3))
  }

  "ScoresTable" should "Parse games from a list of lines" in new ScoresFixture {
    games.length shouldEqual 5
    games.filter(_.teamOne == Lions).length shouldEqual 3
    games.zip(gamesFixture) foreach { case (game, fixtureGame) =>
      game shouldEqual fixtureGame
    }
  }

  "ScoresTable" should "Rank teams correctly" in new ScoresFixture {
    val rankedTeams = new ScoresTable(games).pointRecordsWithRank
    rankedTeams shouldEqual rankedTeamsResultFixture
    rankedTeams.foreach(println)
  }
}
