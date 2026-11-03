package game

import monix.eval.Task

case class MaxNode(
    perspective: Perspective,
    game: Game,
    depthRemaining: Int,
    numSimulations: Int,
    debug: Boolean = false,
) extends MinMaxNode
    with EvalNode {

  def asyncBestAction(
    limitToMoves: List[Action] = List(),
    onScoredMoves: List[(Score, Action)] => Unit = _ => (),
  ): Task[(Score, Action)] =
    asyncGetScoredMoves(limitToMoves).map { scoredMoves =>
      onScoredMoves(scoredMoves)
      if (debug) printScoredMoves(scoredMoves)

      selectBestMove(scoredMoves).getOrElse((Score(0.0), Pass))
    }

  // Defers each move's evaluation so parSequence can run them concurrently.
  def asyncGetScoredMoves(
    limitToMoves: List[Action] = List(),
  ): Task[List[(Score, Action)]] = {
    val tasks = possibleFutures()
      .filter { case (_, move) =>
        limitToMoves.isEmpty || limitToMoves.contains(move)
      }
      .map { case (future, move) =>
        Task {
          val score = AvgNode(
            perspective,
            game,
            future,
            move,
            depthRemaining - 1,
            childSimulations,
          ).score()

          (score, move)
        }
      }

    Task.parSequence(tasks)
  }

  def isBetterScore(
      candidate: Score,
      previousBest: Score,
      margin: Double = 0.0,
  ): Boolean = {
    candidate.value + margin > previousBest.value
  }

  // reduce breadth only in max nodes to gradually reduce tree width as depth increases
  override protected def childSimulations: Int = Config.nextBreadth(numSimulations)

  private def printScoredMoves(
    scoredMoves: List[(Score, Action)],
  ): Unit = {
    println("MaxNode Debug scored moves ------------------")
    println(s"Depth: $depthRemaining")
    println(s"Num Simulations: $numSimulations")

    scoredMoves
      .sortBy(_._1.value)
      .reverse
      .foreach { case (score, move) =>
        println(s"$move: ${score.value}")
      }
  }
}
