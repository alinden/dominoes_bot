package game

import scala.util.Random

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

object Robot {
  def getRandomLegalAction(game: Game): Option[Action] = {
    val options = game.allLegalActions()
    Random.shuffle(options).headOption
  }

  def greedyMove(game: Game): Option[Action] = {
    val scoringMoves: List[(Move, Int)] = game.scoringMoves()
    val legalScoringMoves: List[(Move, Int)] = scoringMoves
      .filter {
        case (move, _) => {
          game.activePlayer().hand.contains(move.domino)
        }
      }
    if (!legalScoringMoves.isEmpty) {
      return legalScoringMoves.headOption.map(_._1.asInstanceOf[Action])
    }
    getRandomLegalAction(game)
  }

  def getBestMove(
      game: Game,
      guess: EliminationHand,
      debug: Boolean,
      depth: Int,
      numSimulations: Int,
      limitToMoves: List[Action] = List(),
      onScoredMoves: List[(Score,Action)] => Unit = _ => (),
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
    println("")
    println(game)
    if (!debug) Thread.sleep(500)
    println(".")
    if (!debug) Thread.sleep(500)
    println("..")
    if (!debug) Thread.sleep(500)
    println("...")
    val distinctMoves = game.simplify(game.legalMoves()).length
    if (distinctMoves <= 1) {
      if (!debug && distinctMoves == 1) {
        // Faking thinking to not give away having no options.
        val sleepSeconds = Random.nextInt(12) * 1000
        Thread.sleep(sleepSeconds)
      }
      return getRandomLegalAction(game)
    } else {
      // Call getBestMove with the following params until it takes longer than
      // expectedTime to get a result. This should lead to quicker first turns
      // and better later turns.
      val moveParams = List((5, 30), (7, 30), (9, 30), (11, 30))
      val expectedTime = 3000.0
      var dt = 0.0
      var i = 0
      var move: Action = null
      while ((dt < expectedTime) && (i < moveParams.length)) {
        val (depth, simulations) = moveParams(i)
        SearchStats.reset()
        val t1 = System.nanoTime()

        val result = getBestMove(
          game,
          guess,
          debug,
          depth,
          simulations,
          onScoredMoves = scoredMoves => {
            decisionLog.foreach { log =>
              log.println(s"Depth: $depth, Simulations: $simulations")

              scoredMoves.sortBy(-_._1.value).foreach {
                case (score, action) =>
                  log.println(
                    f"  $action%-25s " +
                    f"mean=${score.value}%.3f " +
                    f"stddev=${score.stddev}%.3f " +
                    f"min=${score.min()}%.3f " +
                    f"max=${score.max()}%.3f " +
                    f"pLoss=${1.0 - score.positiveLikelihood(-1.0)}%.3f " +
                    f"pWin=${score.positiveLikelihood(1.0)}%.3f"
                  )
              }
            }
          },
        ).runSyncUnsafe()

        move = result._2

        val t2 = System.nanoTime()
        dt = (t2 - t1) / 1000000.0

        val nodes = SearchStats.count()
        val nodesPerSecond =
          if (dt > 0) nodes * 1000.0 / dt else 0.0

        decisionLog.foreach { log =>
          log.println(f"AvgNodes evaluated: $nodes%,d")
          log.println(f"AvgNodes/second: $nodesPerSecond%,.0f")
          log.flush()
        }

        decisionLog.foreach { log =>
          log.println(f"Selected at depth $depth: $move (${result._1.value}%.3f)")
          log.println(f"Elapsed: $dt%.0f ms")
          log.println()
          log.flush()
        }

        i = i + 1
      }
      decisionLog.foreach { log =>
        log.println(s"FINAL DECISION: $move")
        log.println("=" * 60)
        log.println()
        log.flush()
      }
      Some(move)
    }
  }
}
