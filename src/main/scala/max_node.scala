package game

case class MaxNode(
  perspective: Perspective,
  game: Game,
  depthRemaining: Int,
  numSimulations: Int,
  debug: Boolean = false,
) extends MinMaxNode with EvalNode {
  def isBetterScore(candidate: Double, previousBest: Double, margin: Double = 0.0): Boolean = {
    candidate + margin > previousBest
  }
  def getScoredMoves(limitToMoves: List[Action] = List()): List[(Double,Action)] = {
    val ret = possibleFutures()
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
    if (debug) {
      println("MaxNode Debug scored moves ------------------")
      println(s"Depth: ${depthRemaining}")
      println(s"Num Simulations: ${numSimulations}")
      ret.sortBy(_._1).reverse.map{ case (score, move) => s"${move}: ${score}" }.foreach(println(_))
    }
    ret
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
