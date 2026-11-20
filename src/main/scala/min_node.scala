package game

case class MinNode(
    perspective: Perspective,
    game: Game,
    depthRemaining: Int,
    numSimulations: Int,
    isRoot: Boolean = false,
) extends MinMaxNode
    with EvalNode {
  def isBetterScore(
      candidate: Score,
      previousBest: Score,
      margin: Double = 0.0,
  ): Boolean = {
    candidate.value - margin < previousBest.value
  }

  override protected def childSimulations: Int = numSimulations

  def scoredMoves(possibleMoves: List[Move]): List[(ScoreEstimate, Move)] =
    possibleMoves.flatMap { move =>
      game.act(move, false, true).map { future =>
        val scoreEstimate = AvgNode(
          perspective,
          game,
          future,
          move,
          depthRemaining - 1,
          childSimulations
        ).scoreEstimate()

        (scoreEstimate, move)
      }
    }

  def goodMoves(scoredMoves: List[(ScoreEstimate, Move)]): List[Move] = {
    val confidence = 2.0

    val sortedMoves = scoredMoves.sortBy(_._1.mean.value)
    val bestScore = sortedMoves.head._1

    sortedMoves
      .filter { case (score, _) =>
        val difference = score.mean.value - bestScore.mean.value

        val combinedSE = math.sqrt(
          math.pow(score.standardError, 2) +
          math.pow(bestScore.standardError, 2)
        )

        difference <= confidence * combinedSE
      }
      .map(_._2)
  }

}
