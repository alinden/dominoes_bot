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

  def scoredMoves(possibleMoves: List[Move]): List[(Score, Move)] =
    possibleMoves.flatMap { move =>
      game.act(move, false, true).map { future =>
        val score = AvgNode(
          perspective,
          game,
          future,
          move,
          depthRemaining - 1,
          childSimulations
        ).score()

        (score, move)
      }
    }

  def goodMoves(scoredMoves: List[(Score, Move)]): List[Move] = {
    val margin = 0.02

    val sortedMoves = scoredMoves.sortWith {
      case ((xScore, _), (yScore, _)) =>
        isBetterScore(xScore, yScore)
    }

    val bestScore = sortedMoves.head._1

    sortedMoves
      .filter { case (score, _) =>
        isBetterScore(score, bestScore, margin)
      }
      .map(_._2)
  }

}
