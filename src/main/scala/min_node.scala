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
      limitToMoves: List[Action] = List()
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
    val after = game.act(action, false, true)
    if (action == Draw) {
      assert(false, "drawing in min node")
      after.flatMap((g) => {
        MinNode(
          perspective.update(game, g),
          g,
          depthRemaining - 1,
          numSimulations,
        ).toMaxNode()
      })
    } else {
      after.map((g) => {
        MaxNode(
          perspective.update(game, g).swap(),
          g,
          depthRemaining - 1,
          numSimulations,
        )
      })
    }
  }
}
