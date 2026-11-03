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
  ): Task[(Score, Action)] = {
    asyncGetScoredMoves(limitToMoves).map((scoredMoves) => {
      onScoredMoves(scoredMoves)

      if (debug) {
        println("MaxNode Debug scored moves ------------------")
        println(s"Depth: ${depthRemaining}")
        println(s"Num Simulations: ${numSimulations}")
        scoredMoves
          .sortBy(_._1.value)
          .reverse
          .foreach { case (score, move) =>
            println(s"$move: ${score.value}")
          }
      }
      scoredMoves
        .foldRight[Option[(Score, Action)]](None) { case ((score, move), z) =>
          z match {
            case None => Some((score, move))
            case Some((zScore, zMove)) =>
              if (isBetterScore(score, zScore)) Some((score, move)) else z
          }
        }
        .getOrElse(Score(0.0), Pass)
    })
  }
  def asyncGetScoredMoves(
      limitToMoves: List[Action] = List()
  ): Task[List[(Score, Action)]] = {
    Task.parSequence(
      possibleFutures()
        .withFilter {
          case (_, move) => {
            limitToMoves.isEmpty || limitToMoves.contains(move)
          }
        }
        .map {
          case (future, move) => {
            Task(
              AvgNode(
                perspective,
                game,
                future,
                move,
                depthRemaining - 1,
                Config.nextBreadth(numSimulations),
              ).score(),
              move,
            )
          }
        }
    )
  }

  def isBetterScore(
      candidate: Score,
      previousBest: Score,
      margin: Double = 0.0,
  ): Boolean = {
    candidate.value + margin > previousBest.value
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
              Config.nextBreadth(numSimulations),
            ).score(),
            move,
          )
        }
      }
  }

  def toMinNode(): Option[MinNode] = {
    val (_, action) = bestAction()
    val after = game.act(action, false, true)
    if (action == Draw) {
      assert(false, "drawing in max node")
      after.flatMap((g) =>
        MaxNode(
          perspective.update(game, g),
          g,
          depthRemaining - 1,
          Config.nextBreadth(numSimulations),
        ).toMinNode()
      )
    } else {
      after.map((g) =>
        MinNode(
          perspective.update(game, g).swap(),
          g,
          depthRemaining - 1,
          Config.nextBreadth(numSimulations),
        )
      )
    }
  }
}
