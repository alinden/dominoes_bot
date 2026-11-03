package game

import scala.annotation.tailrec
import scala.util.Random

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

object Robot {
  def getRandomLegalAction(game: Game): Option[Action] = {
    val options = game.allLegalActions()
    Random.shuffle(options).headOption
  }

  def greedyMove(game: Game): Option[Action] = {
    val scoringMoves = game.scoringMoves()
      .collect {
        case (move, _) if game.activePlayer().hand.contains(move.domino) => move
      }

    game.simplify(scoringMoves).headOption
      .orElse(getRandomLegalAction(game))
  }

  def getBestMove(
      game: Game,
      guess: EliminationHand,
      debug: Boolean,
      depth: Int,
      numSimulations: Int,
      limitToMoves: List[Action] = List(),
      onScoredMoves: List[(Score, Action)] => Unit = _ => (),
  ): Task[(Score, Action)] = {
    val robotName = "robot"
    val robotHand = game.playerByName(robotName).hand
    val perspective = Perspective(
      robotName,
      "human",
      robotHand,
      None,
      guess,
      None,
    )
    val maxNode = MaxNode(perspective, game, depth, numSimulations, debug)
    maxNode.asyncBestAction(limitToMoves, onScoredMoves)
  }

  def act(
      game: Game,
      guess: EliminationHand,
      debug: Boolean,
      decisionLog: Option[java.io.PrintWriter] = None,
  ): Option[Action] = {
    showThinking(game, debug)
    val distinctMoves = game.simplify(game.legalMoves()).length
    if (distinctMoves <= 1) {
      if (!debug && distinctMoves == 1) {
        // Faking thinking to not give away having no options.
        val sleepSeconds = Random.nextInt(12) * 1000
        Thread.sleep(sleepSeconds)
      }
      return getRandomLegalAction(game)
    } else {
      val move = searchUntilTimeout(
        game,
        guess,
        debug,
        decisionLog,
        Config.moveParams.toList,
      )

      move.foreach { action =>
        decisionLog.foreach(logFinalDecision(_, action))
      }

      move
    }
  }

  private def showThinking(game: Game, debug: Boolean): Unit = {
    println("")
    println(game)

    if (!debug) Thread.sleep(500)
    println(".")
    if (!debug) Thread.sleep(500)
    println("..")
    if (!debug) Thread.sleep(500)
    println("...")
  }

  private def logScoredMoves(
    log: java.io.PrintWriter,
    depth: Int,
    simulations: Int,
    scoredMoves: List[(Score, Action)],
  ): Unit = {
    log.println(s"Depth: $depth, Simulations: $simulations")

    scoredMoves.sortBy(-_._1.value).foreach { case (score, action) =>
      log.println(f"$action%-25s mean=${score.value}%.3f ")
    }
  }

  private def logSearchResult(
    log: java.io.PrintWriter,
    depth: Int,
    result: (Score, Action),
    elapsedMs: Double,
  ): Unit = {
    val nodes = SearchStats.count()
    val drawNodes = SearchStats.drawCount()
    val drawSimulations = SearchStats.drawSimulationCount()
    val nodesPerSecond =
      if (elapsedMs > 0) nodes * 1000.0 / elapsedMs else 0.0

    log.println(f"AvgNodes evaluated: $nodes%,d")
    log.println(f"AvgNodes/second: $nodesPerSecond%,.0f")
    log.println(f"Draw nodes: $drawNodes%,d")
    log.println(f"Draw simulations: $drawSimulations%,d")
    log.flush()

    log.println(f"Selected at depth $depth: ${result._2} (${result._1.value}%.3f)")
    log.println(f"Elapsed: $elapsedMs%.0f ms")
    log.println()
    log.flush()
  }

  private def logFinalDecision(
    log: java.io.PrintWriter,
    move: Action,
  ): Unit = {
    log.println(s"FINAL DECISION: $move")
    log.println("=" * 60)
    log.println()
    log.flush()
  }

  @tailrec
  private def searchUntilTimeout(
    game: Game,
    guess: EliminationHand,
    debug: Boolean,
    decisionLog: Option[java.io.PrintWriter],
    remainingParams: List[(Int, Int)],
    previousElapsedMs: Double = 0.0,
    previousMove: Option[Action] = None,
  ): Option[Action] = {
    if (previousElapsedMs >= Config.moveTimeMs || remainingParams.isEmpty) {
      previousMove
    } else {
      val (depth, simulations) = remainingParams.head

      SearchStats.reset()
      val start = System.nanoTime()

      val result = getBestMove(
        game,
        guess,
        debug,
        depth,
        simulations,
        onScoredMoves = scoredMoves =>
          decisionLog.foreach(logScoredMoves(_, depth, simulations, scoredMoves)),
      ).runSyncUnsafe()

      val elapsedMs = (System.nanoTime() - start) / 1000000.0

      decisionLog.foreach(logSearchResult(_, depth, result, elapsedMs))

      searchUntilTimeout(
        game,
        guess,
        debug,
        decisionLog,
        remainingParams.tail,
        elapsedMs,
        Some(result._2),
      )
    }
  }
}
