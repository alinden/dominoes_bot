package game

import monix.eval.Task

case class MaxNode(
  perspective: Perspective,
  game: Game,
  depthRemaining: Int,
  numSimulations: Int,
  debug: Boolean = false,
) extends MinMaxNode with EvalNode {
  def asyncBestAction(limitToMoves: List[Action] = List()): Task[(Score,Action)] = {
    asyncGetScoredMoves(limitToMoves).map((scoredMoves) => {
      if (debug) {
        println("MaxNode Debug scored moves ------------------")
        println(s"Depth: ${depthRemaining}")
        println(s"Num Simulations: ${numSimulations}")
        def firstFourLetters(d: Double): String = {
          var x = d.toString().take(5)
          while(x.length < 5) {
            x = x + " "
          }
          x
        }
        scoredMoves.sortBy(_._1.value).reverse
          .map{ case (score, move) =>
            s"${move} (mean, stddev, max, min, pLosi, pWinn, pMore): ${
              List(
                score.value,
                score.stddev,
                score.max,
                score.min,
                (1.0 - score.positiveLikelihood(-1.0)),
                score.positiveLikelihood(1.0),
                score.positiveLikelihood(0.0),
              ).map(firstFourLetters(_))
            }"
          }.foreach(println(_))
      }
      scoredMoves.foldRight[Option[(Score,Action)]](None){ case ((score, move), z) => z match {
        case None => Some((score, move))
        case Some((zScore, zMove)) => if (isBetterScore(score, zScore)) Some((score, move)) else z
      }}.getOrElse(Score(0.0),Pass)
    })
  }
  def asyncGetScoredMoves(limitToMoves: List[Action] = List()): Task[List[(Score,Action)]] = {
    Task.parSequence(
      possibleFutures()
        .withFilter{ case (_, move) => {
          limitToMoves.isEmpty || limitToMoves.contains(move)
        }}
        .map{ case (future, move) => {
        Task(AvgNode(
            perspective,
            game,
            future,
            move,
            depthRemaining - 1,
            numSimulations,
        ).score(), move)
      }}
    )
  }

  def isBetterScore(candidate: Score, previousBest: Score, margin: Double = 0.0): Boolean = {
    candidate.value + margin > previousBest.value
  }
  def getScoredMoves(limitToMoves: List[Action] = List()): List[(Score,Action)] = {
    possibleFutures()
      .withFilter{ case (_, move) => {
        limitToMoves.isEmpty || limitToMoves.contains(move)
      }}
      .map{ case (future, move) => {
      (AvgNode(
          perspective,
          game,
          future,
          move,
          depthRemaining - 1,
          numSimulations,
      ).score(), move)
    }}
  }

  def toMinNode(): Option[MinNode] = {
    val (_, action) = bestAction()
    val after = game.act(action, false, true)
    if (action == Draw) {
      after.flatMap((g) =>
        MaxNode(
          perspective.update(game, g),
          g,
          depthRemaining - 1,
          numSimulations,
        ).toMinNode()
      )
    } else {
      after.map((g) =>
        MinNode(
          perspective.update(game, g).swap(),
          g,
          depthRemaining - 1,
          numSimulations,
        )
      )
    }
  }
}
