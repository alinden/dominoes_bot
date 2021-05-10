package game

import scala.util.Random

object Robot {
  def getRandomLegalAction(game: Game): Option[Action] = {
    val options = game.allLegalActions()
    Random.shuffle(options).headOption
  }

  def greedyMove(game: Game): Option[Action] = {
    val scoringMoves: List[(Move,Int)] = game.scoringMoves()
    val legalScoringMoves: List[(Move,Int)] = scoringMoves
      .filter{ case (move, _) => {
        game.activePlayer().hand.contains(move.domino)
      }}
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
  ): (Score, Action) = {
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
    maxNode.bestAction(limitToMoves)
  }

  def act(
    game: Game,
    guess: EliminationHand,
    debug: Boolean,
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
        val sleepSeconds = Random.nextInt(12)*1000
        Thread.sleep(sleepSeconds)
      }
      return getRandomLegalAction(game)
    } else {
      // Call getBestMove with the following params until it takes longer than
      // expectedTime to get a result. This should lead to quicker first turns
      // and better later turns.
      // Actual time will be around 3x expected time with first moves taking
      // much longer. With (5,20), the first turn takes around 2 mins.
      val moveParams = List((5,20),(7,20),(9,30),(10,30))
      val expectedTime = 3000.0
      var dt = 0.0
      var i = 0
      var move: Action = null
      while((dt < expectedTime) && (i < moveParams.length)) {
        val t1 = System.nanoTime()/1000000.0
        move = getBestMove(game, guess, debug, moveParams(i)._1, moveParams(i)._2)._2
        val t2 = System.nanoTime()/1000000.0
        dt = t2 - t1
        i = i + 1
      }
      Some(move)
    }
  }
}
