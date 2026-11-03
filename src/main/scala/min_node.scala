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
  def getScoredMoves(
      limitToMoves: List[Action] = List(),
  ): List[(Score, Action)] = {
    possibleFutures()
      .withFilter {
        case (_, move) => {
          limitToMoves.isEmpty || limitToMoves.contains(move)
        }
      }
      .map {
        case (future, move) => {
          (
            AvgNode(
              perspective,
              game,
              future,
              move,
              depthRemaining - 1,
              numSimulations,
            ).score(),
            move,
          )
        }
      }
  }

  def toMaxNode(): Option[MaxNode] = {
    val (_, action) = bestAction()
    assert(action != Draw, "drawing in min node")

    game.act(action, false, true).map { next =>
      MaxNode(
        perspective.update(game, next).swap(),
        next,
        depthRemaining - 1,
        numSimulations,
      )
    }
  }
}
