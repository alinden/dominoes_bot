package game

trait MinMaxNode {
  val numSimulations: Int
  val depthRemaining: Int
  val game: Game
  val perspective: Perspective

  protected def childSimulations: Int

  def isBetterScore(
      candidate: Score,
      previousBest: Score,
      margin: Double = 0.0,
  ): Boolean

  def score(): Score = {
    bestScoredMove().map(_._1).getOrElse(Score(0.toFloat))
  }

  def bestAction(limitToMoves: List[Action] = List()): (Score, Action) = {
    bestScoredMove(limitToMoves).getOrElse((Score(0.0), Pass))
  }

  def bestScoredMove(
      limitToMoves: List[Action] = List(),
  ): Option[(Score, Action)] =
    selectBestMove(getScoredMoves(limitToMoves))

  protected def selectBestMove(
      scoredMoves: List[(Score, Action)],
  ): Option[(Score, Action)] =
    scoredMoves.foldRight[Option[(Score, Action)]](None) { case ((score, move), best) =>
      best match {
        case None => Some((score, move))
        case Some((bestScore, _)) =>
          if (isBetterScore(score, bestScore))
            Some((score, move))
          else
            best
      }
    }

  def goodActions(): List[Action] = {
    val futures = possibleFutures()
    if (futures.length == 1) {
      List(futures.head._2)
    } else {
      val margin = 0.02
      val sortedMoves = getScoredMoves()
        .sortWith {
          case ((xScore, _), (yScore, _)) => {
            isBetterScore(xScore, yScore)
          }
        }
      val bestScore = sortedMoves.head._1
      sortedMoves
        .withFilter {
          case (score, move) => {
            isBetterScore(score, bestScore, margin)
          }
        }
        .map(_._2)
    }
  }

  def safeGame(): Game = {
    game.withReplacedActiveHand(perspective.hand)
  }

  // unused heuristic. add back in if openings are slow. its probably correct most of
  // the time but also likely possible for the algorithm to determine on its own
  // and with parallel execution, removing a few options might not make it that much
  // faster.
  private def isGoodOpeningMove(game: Game, action: Action): Boolean = action match {
    case Move(tile, _) if game.board.tiles().length == 1 =>
      val badStartingPieces =
        Set(Domino(6, 3), Domino(4, 2), Domino(3, 4), Domino(6, 2))

      val hand = perspective.hand.toSet + tile

      val hasConnectingPiece = Domino.all().exists { d =>
        d != tile &&
        hand.contains(d) &&
        (d.high == tile.high ||
          d.high == tile.low ||
          d.low == tile.high ||
          d.low == tile.low)
      }

      hasConnectingPiece && !badStartingPieces.contains(tile)

    case _ => true
  }

  def possibleFutures(): List[(Game, Action)] = {
    val gameState = safeGame()

    gameState.allLegalActions().flatMap { action =>
      gameState.act(action, false, true).map { next =>
        (next, action)
      }
    }
  }

  def getScoredMoves(
      limitToMoves: List[Action] = List(),
  ): List[(Score, Action)] =
    possibleFutures()
      .filter { case (_, move) =>
        limitToMoves.isEmpty || limitToMoves.contains(move)
      }
      .map { case (future, move) =>
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
