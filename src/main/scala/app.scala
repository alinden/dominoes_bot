import game._

import scala.annotation.tailrec
import scala.concurrent.duration._

case class MatchScore(pOneScore: Int, pTwoScore: Int, bestOf: Int) {
  def isOver: Boolean = {
    val halfPlusOne = math.floor(bestOf.toFloat / 2.0) + 1
    (pOneScore >= halfPlusOne) || (pTwoScore >= halfPlusOne)
  }
}

private case class GameSession(
    game: Game,
    guess: Option[EliminationHand],
    pending: List[Option[EliminationHand] => Option[EliminationHand]] = Nil,
)

object App {
  def playGame(
      mode: String,
      matchScore: MatchScore,
      decisionLog: Option[java.io.PrintWriter] = None,
      turnTimeSeconds: Int = Config.turnTimeSeconds,
      inferenceTimeSeconds: Int = Config.inferenceTimeSeconds,
  ): MatchScore = {
    val debug = mode == "--play_greedy"
    val initialGame = Game.setup("human", "robot")

    showGameStart(initialGame, matchScore.bestOf, debug)

    val initialSession = GameSession(initialGame, None)
    val finalSession =
      playUntilFinished(initialSession, mode, debug, decisionLog, turnTimeSeconds, inferenceTimeSeconds)
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
      turnTimeSeconds: Int,
      inferenceTimeSeconds: Int,
  ): Unit = {
    if (!matchScore.isOver) {
      val updatedScore = playGame(mode, matchScore, decisionLog, turnTimeSeconds, inferenceTimeSeconds)
      println(updatedScore)
      playMatch(mode, updatedScore, decisionLog, turnTimeSeconds, inferenceTimeSeconds)
    }
  }

  def main(args: Array[String]): Unit = {
    val validModes = Set("--play_human", "--play_greedy")

    val options = args.drop(1).grouped(2).toList
    val logFile = options.collectFirst { case Array("--log-decisions", filename) => filename }
    val timeOption = options.collectFirst { case Array("--turn-time-seconds", value) => value }
    val inferenceOption = options.collectFirst { case Array("--inference-time-seconds", value) => value }
    val turnTimeSeconds = timeOption match {
      case Some(value) => value.toIntOption.filter(_ >= 0)
      case None => Some(Config.turnTimeSeconds)
    }
    val inferenceTimeSeconds = inferenceOption match {
      case Some(value) => value.toIntOption.filter(_ > 0)
      case None => Some(Config.inferenceTimeSeconds)
    }
    val validOptions = options.forall(option =>
      option.length == 2 &&
        Set("--log-decisions", "--turn-time-seconds", "--inference-time-seconds").contains(option(0))
    ) && options.map(_.head).distinct.size == options.size
    if (!args.headOption.exists(validModes.contains) || !validOptions ||
        turnTimeSeconds.isEmpty || inferenceTimeSeconds.isEmpty) {
      println(
        """Usage: sbt "run [--play_human|--play_greedy] [--log-decisions filename] [--turn-time-seconds N] [--inference-time-seconds N]"""",
      )
      return
    }

    val mode = args.head
    val decisionLog = logFile.map { filename =>
      new java.io.PrintWriter(new java.io.FileWriter(filename))
    }

    try {
      val bestOf = if (mode == "--play_human") 3 else 39
      val initialScore = MatchScore(0, 0, bestOf)

      playMatch(mode, initialScore, decisionLog, turnTimeSeconds.get, inferenceTimeSeconds.get)
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
      log.println(hand.decisionLogSummary(game.playerByName("human").hand.toSet))
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
      EliminationHand.createFromOpponentHand("human", game.playerByName("robot").hand)
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
            .updateForEntry(entry, previous, false, true),
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
            updated.eliminateTile(current.playerByName("robot").hand.head)
          else
            updated,
        )

      case None => None
    }

  private def playTurn(
      session: GameSession,
      mode: String,
      debug: Boolean,
      decisionLog: Option[java.io.PrintWriter],
      turnTimeSeconds: Int,
      inferenceTimeSeconds: Int,
  ): Option[GameSession] = {
    val game = session.game
    val capped = turnTimeSeconds > 0

    if (game.turn == HighestDouble) {
      println("")
      println("First Turn, playing the highest double.")
      if (!debug) Thread.sleep(2000)

      val nextGame = game
        .playHighestDouble()
        .orElse(Some(game.reshuffle()))

      nextGame.map { next =>
        observe(session, next, capped)(updateGuessForOpening(_, game, next))
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
        observe(session, next, capped)(updateGuessForOpponent(_, game, next))
      }

    } else {
      Robot.showThinking(game, debug)
      val (prepared, action) =
        chooseScalaAction(session, debug, decisionLog, turnTimeSeconds, inferenceTimeSeconds)

      game.act(action, true).map { next =>
        observe(prepared, next, capped)(updateGuessForRobot(_, game, next))
      }
    }
  }

  private def chooseScalaAction(
      session: GameSession,
      debug: Boolean,
      decisionLog: Option[java.io.PrintWriter],
      turnTimeSeconds: Int,
      inferenceTimeSeconds: Int,
  ): (GameSession, Action) = {
    val game = session.game
    val capped = turnTimeSeconds > 0
    val inferenceDeadline = Option.when(capped)(TurnDeadline.after(inferenceTimeSeconds.seconds))
    val (prepared, inferred) = TurnDeadline.withCurrent(inferenceDeadline) {
      val ready = applyObservations(session)
      val guess = if (ready.pending.nonEmpty) None
      else TurnDeadline.attempt {
        TurnDeadline.check()
        getOrCreateGuess(ready.guess, game)
      }
      (ready, if (capped) guess.filter(_.possibilities.nonEmpty) else guess)
    }

    // Keep the fallback local to this decision. Pending evidence can still be applied next turn.
    val guess = inferred.orElse(Option.when(capped)(Robot.sampleUnknownHand(game)))
    decisionLog.foreach { log =>
      if (capped && inferred.isEmpty) log.println("Using a sampled hand estimate for this decision.")
      logRobotTurn(log, game, guess)
    }
    val searchDeadline = Option.when(capped)(TurnDeadline.after(turnTimeSeconds.seconds))
    val action = TurnDeadline.withCurrent(searchDeadline) {
      Robot.act(game, guess, debug, decisionLog).get
    }

    (prepared, action)
  }

  def playCppGame(
      cpp: CppMatchBot,
      cppSeat: Int,
      dealSeed: Int,
      searchSeconds: Int,
      inferenceSeconds: Int,
      decisionLog: Option[java.io.PrintWriter],
      otherCpp: Option[CppMatchBot] = None,
  ): (Int, Int) = {
    val random = new scala.util.Random(dealSeed)
    val (first, second) = if (cppSeat == 0) ("human", "robot") else ("robot", "human")
    var session = GameSession(
      Game(Board.empty(), Player(first, Nil, 0), Player(second, Nil, 0), HighestDouble, Nil,
        GameLog.empty()),
      None,
    )
    var dealNeeded = true
    var turns = 0
    val bots = cpp :: otherCpp.toList
    val firstLabel = if (otherCpp.isDefined) s"Depth ${cpp.depth}" else "C++"
    val secondLabel = otherCpp.map(bot => s"Depth ${bot.depth}").getOrElse("Scala")
    while (!session.game.isOver) {
      if (dealNeeded) {
        val tiles = random.shuffle(Domino.all())
        val game = session.game
        val dealt = game.copy(board = Board.empty(), playerOne = game.playerOne.copy(hand = tiles.take(7)),
          playerTwo = game.playerTwo.copy(hand = tiles.slice(7, 14)), boneyard = tiles.drop(14),
          log = GameLog.empty(), lastTurnPass = false)
        bots.foreach(_.deal(tiles, dealt))
        session = GameSession(dealt, None)
        dealNeeded = false
      }

      val game = session.game
      turns += 1
      if (game.turn == HighestDouble) {
        val opening = Domino.doubles().reverseIterator.flatMap { tile =>
          game.playHighestDouble(tile).map { case (next, _) =>
            next.log(HighestDoubleEntry(next.inactivePlayer().name, tile))
          }
        }.take(1).toList.headOption
        val next = opening.getOrElse(game)
        bots.foreach(_.advance(PlayHighestDouble, next))
        dealNeeded = opening.isEmpty
        if (!dealNeeded) {
          session = if (otherCpp.isDefined) GameSession(next, None)
                    else observe(session, next, true)(updateGuessForOpening(_, game, next))
        }
      } else {
        val secondBotTurn = game.activePlayer().name == "robot"
        val cppTurn = if (secondBotTurn) otherCpp else Some(cpp)
        val started = System.nanoTime()
        val (prepared, action) = cppTurn match {
          case Some(bot) => (session, bot.choose(turns))
          case None =>
            decisionLog.foreach { log => log.println(s"TURN $turns"); log.flush() }
            val decision = chooseScalaAction(session, false, decisionLog, searchSeconds, inferenceSeconds)
            decisionLog.foreach { log => log.println(s"Applied: ${decision._2}"); log.flush() }
            decision
        }
        val elapsed = (System.nanoTime() - started) / 1000000.0
        val next = game.act(action, debug = false, dontShuffle = true)
          .getOrElse(throw new IllegalStateException(s"Illegal bot action: $action"))
        bots.foreach(_.advance(action, next))
        val bot = if (secondBotTurn) secondLabel else firstLabel
        println(f"$turns%3d $bot%-5s $action%-24s $elapsed%9.1f ms  " +
          s"$firstLabel ${next.playerByName("human").score} / $secondLabel ${next.playerByName("robot").score}")

        dealNeeded = !next.isOver && (next.turn == HighestDouble ||
          next.playerOne.hand.isEmpty || next.playerTwo.hand.isEmpty)
        session = if (dealNeeded || next.isOver || otherCpp.isDefined) GameSession(next, None)
        else if (secondBotTurn) observe(prepared, next, true)(updateGuessForRobot(_, game, next))
        else observe(prepared, next, true)(updateGuessForOpponent(_, game, next))
      }
    }
    val finalGame = session.game
    (finalGame.playerByName("human").score, finalGame.playerByName("robot").score)
  }

  private def observe(session: GameSession, next: Game, capped: Boolean)(
      update: Option[EliminationHand] => Option[EliminationHand],
  ): GameSession = {
    if (next.log.log.isEmpty || next.isOver) GameSession(next, None)
    else if (capped) session.copy(game = next, pending = session.pending :+ update)
    else GameSession(next, update(session.guess))
  }

  @tailrec
  private def applyObservations(session: GameSession): GameSession = session.pending match {
    case update :: remaining =>
      TurnDeadline.attempt {
        TurnDeadline.check()
        update(session.guess)
      } match {
        case Some(guess) => applyObservations(session.copy(guess = guess, pending = remaining))
        case None => session
      }
    case Nil => session
  }

  @tailrec
  private def playUntilFinished(
      session: GameSession,
      mode: String,
      debug: Boolean,
      decisionLog: Option[java.io.PrintWriter],
      turnTimeSeconds: Int,
      inferenceTimeSeconds: Int,
  ): GameSession = {
    if (session.game.isOver) {
      session
    } else {
      val previous = session.game
      val next = playTurn(session, mode, debug, decisionLog, turnTimeSeconds, inferenceTimeSeconds).get

      showGuess(next.guess, debug)
      showPointsScored(previous, next.game)
      println("")

      playUntilFinished(next, mode, debug, decisionLog, turnTimeSeconds, inferenceTimeSeconds)
    }
  }
}
