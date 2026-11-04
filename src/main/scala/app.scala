import game._

import scala.annotation.tailrec

case class MatchScore(pOneScore: Int, pTwoScore: Int, bestOf: Int) {
  def isOver: Boolean = {
    val halfPlusOne = math.floor(bestOf.toFloat / 2.0) + 1
    (pOneScore >= halfPlusOne) || (pTwoScore >= halfPlusOne)
  }
}

private case class GameSession(
    game: Game,
    guess: Option[EliminationHand],
)

object App {
  def playGame(
    mode: String,
    matchScore: MatchScore,
    decisionLog: Option[java.io.PrintWriter] = None,
  ): MatchScore = {
    val debug = mode == "--play_greedy"
    val initialGame = Game.setup("human", "robot")

    showGameStart(initialGame, matchScore.bestOf, debug)

    val initialSession = GameSession(initialGame, None)
    val finalSession = playUntilFinished(initialSession, mode, debug, decisionLog)
    val game = finalSession.game

    println("game over, result:")
    println(game)

    if (game.playerOne.score > game.playerTwo.score) {
      MatchScore(
        matchScore.pOneScore + 1,
        matchScore.pTwoScore,
        matchScore.bestOf,
      )
    } else {
      MatchScore(
        matchScore.pOneScore,
        matchScore.pTwoScore + 1,
        matchScore.bestOf,
      )
    }
  }

  @annotation.tailrec
  private def playMatch(
      mode: String,
      matchScore: MatchScore,
      decisionLog: Option[java.io.PrintWriter],
  ): Unit = {
    if (matchScore.isOver) {
      matchScore
    } else {
      val updatedScore = playGame(mode, matchScore, decisionLog)
      println(updatedScore)
      playMatch(mode, updatedScore, decisionLog)
    }
  }

  def main(args: Array[String]): Unit = {
    val validModes = Set("--play_human", "--play_greedy")

    if (
      args.isEmpty ||
      !validModes.contains(args.head) ||
      (args.length != 1 &&
        !(args.length == 3 && args(1) == "--log-decisions"))
    ) {
      println(
        """Usage: sbt "run [--play_human|--play_greedy] [--log-decisions filename]"""",
      )
      return
    }

    val mode = args.head
    val logFile = if (args.length == 3) Some(args(2)) else None

    val decisionLog = logFile.map { filename =>
      new java.io.PrintWriter(new java.io.FileWriter(filename))
    }

    try {
      val bestOf = if (mode == "--play_human") 3 else 39
      val initialScore = MatchScore(0, 0, bestOf)

      playMatch(mode, initialScore, decisionLog)
    } finally {
      decisionLog.foreach(_.close())
    }
  }

  private def showGameStart(game: Game, bestOf: Int, debug: Boolean): Unit = {
    println("New Game!")
    println(s"Playing a best of $bestOf")
    println("")
    println(game)
    if (!debug) Thread.sleep(2000)
  }

  private def showGuess(guess: Option[EliminationHand], debug: Boolean): Unit = {
    if (debug) {
      println("--guess--")
      println(guess.getOrElse("no guess"))
      println("--guess--")
    }
  }

  private def logRobotTurn(
    log: java.io.PrintWriter,
    game: Game,
    guess: Option[EliminationHand],
  ): Unit = {
    log.println("=" * 60)
    log.println("ROBOT TURN")
    log.println(game)
    log.println("Opponent hand estimates:")

    guess.foreach { hand =>
      log.println(hand.decisionLogSummary(game.playerOne.hand.toSet))
    }

    log.println()
    log.flush()
  }

  private def showPointsScored(previous: Game, current: Game): Unit = {
    if (previous.playerOne.score < current.playerOne.score) {
      val points = current.playerOne.score - previous.playerOne.score
      println(s"${previous.playerOne.name}: score ${points} points")
    } else if (previous.playerTwo.score < current.playerTwo.score) {
      val points = current.playerTwo.score - previous.playerTwo.score
      println(s"${previous.playerTwo.name}: score ${points} points")
    }
  }

  private def getOrCreateGuess(
    guess: Option[EliminationHand],
    game: Game,
  ): EliminationHand =
    guess.getOrElse {
      EliminationHand.createFromOpponentHand("human", game.playerTwo.hand)
    }

  private def updateGuessForOpening(
    guess: Option[EliminationHand],
    previous: Game,
    current: Game,
  ): Option[EliminationHand] = {
    current.log.log.headOption
      .filter(_.action == PlayHighestDouble)
      .map { entry =>
        getOrCreateGuess(guess, current).updateForEntry(entry, previous)
      }
      .orElse(guess)
  }

  private def updateGuessForOpponent(
    guess: Option[EliminationHand],
    previous: Game,
    current: Game,
  ): Option[EliminationHand] =
    current.log.log.headOption match {
      case Some(entry) =>
        Some(
          getOrCreateGuess(guess, current)
            .updateForEntry(entry, previous, false, true)
        )
      case None => None
    }

  private def updateGuessForRobot(
    guess: Option[EliminationHand],
    previous: Game,
    current: Game,
): Option[EliminationHand] =
  current.log.log.headOption match {
    case Some(entry) =>
      val updated =
        getOrCreateGuess(guess, current).updateForEntry(entry, previous)

      Some(
        if (entry.action == Draw)
          updated.eliminateTile(current.playerTwo.hand.head)
        else
          updated
      )

    case None => None
  }

  private def playTurn(
    session: GameSession,
    mode: String,
    debug: Boolean,
    decisionLog: Option[java.io.PrintWriter],
  ): Option[GameSession] = {
    val game = session.game
    val guess = session.guess

    if (game.turn == HighestDouble) {
      println("")
      println("First Turn, playing the highest double.")
      if (!debug) Thread.sleep(2000)

      val nextGame = game.playHighestDouble()
        .orElse(Some(game.reshuffle()))

      nextGame.map { next =>
        GameSession(
          next,
          updateGuessForOpening(guess, game, next),
        )
      }

    } else if (game.turn == PlayerOneTurn) {
      println("")
      println(game)

      val nextGame =
        if (mode == "--play_human") {
          game.getUserInput()
        } else {
          game.act(Robot.greedyMove(game).get, true)
        }

      nextGame.map { next =>
        GameSession(
          next,
          updateGuessForOpponent(guess, game, next),
        )
      }

    } else {
      decisionLog.foreach(logRobotTurn(_, game, guess))

      val action = Robot.act(
        game,
        getOrCreateGuess(guess, game),
        debug,
        decisionLog,
      ).get

      game.act(action, true).map { next =>
        GameSession(
          next,
          updateGuessForRobot(guess, game, next),
        )
      }
    }
  }

  @tailrec
  private def playUntilFinished(
      session: GameSession,
      mode: String,
      debug: Boolean,
      decisionLog: Option[java.io.PrintWriter],
  ): GameSession = {
    if (session.game.isOver) {
      session
    } else {
      val previous = session.game
      val next = playTurn(session, mode, debug, decisionLog).get

      showGuess(next.guess, debug)
      showPointsScored(previous, next.game)
      println("")

      playUntilFinished(next, mode, debug, decisionLog)
    }
  }
}
