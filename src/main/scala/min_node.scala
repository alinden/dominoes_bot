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
}
