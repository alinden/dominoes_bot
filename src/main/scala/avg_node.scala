package game

case class AvgNode(
  perspective: Perspective,
  beforeGame: Game,
  afterGame: Game,
  action: Action,
  depthRemaining: Int,
  numSimulations: Int,
) extends EvalNode {
  /**
   * Score a game that could be complete or ongoing only by looking at
   * its current state combined with the game log.
   */
  def heuristic(): Double = {
    val robotScore = afterGame.playerByName("robot").score.toFloat
    val humanScore = afterGame.playerByName("human").score.toFloat
    val isLockedGame = afterGame.playerOne.hand.isEmpty && afterGame.playerTwo.hand.isEmpty
    val isDominoes = !isLockedGame && (afterGame.playerOne.hand.isEmpty || afterGame.playerTwo.hand.isEmpty)
    val isRobotTurn = afterGame.activePlayer() == afterGame.playerByName("robot")
    (isDominoes, isLockedGame) match {
      case (_, true) => {
        // Locked game.
        robotScore - humanScore
      }
      case (true, _) => {
        // Dominoes.
        val turnBonus = (isRobotTurn, afterGame.isOver) match {
          case (_, true) => if (robotScore > humanScore) 99 else -99
          case (true, false) => 5.0
          case (false, false) => -5.0
        }
        val isRobot = perspective.name == "robot"
        val tileBonus = (isDominoes, isRobotTurn, isRobot) match {
          case (false, _, _) => 0.0
          case (true, false, true) => {
            // robot thinking about human dominoes
            (-1)*perspective.hand.foldRight(0){ (domino, z) =>  {
              domino.high + domino.low + z
            }}
            .toFloat
          }
          case (true, false, false) => {
            // human thinking about human dominoes
            (-1)*((0 to numSimulations)
              .map((_) => {
                val rawCount = perspective.opponentHand.getSample()
                  .foldRight(0){ (domino, z) =>  {
                    domino.high + domino.low + z
                  }}
                  .toFloat
                Math.round(rawCount/5)*5
              })
              .sum
              .toFloat)/numSimulations.toFloat
          }
          case (true, true, true) => {
            // robot thinking about robot dominoes
            ((0 to numSimulations)
              .map((_) => {
                val rawCount = perspective.opponentHand.getSample()
                  .foldRight(0){ (domino, z) =>  {
                    domino.high + domino.low + z
                  }}
                  .toFloat
                Math.round(rawCount/5)*5
              })
              .sum
              .toFloat)/numSimulations.toFloat
          }
          case (true, true, false) => {
            // human thinking about robot dominoes
            perspective.hand.foldRight(0){ (domino, z) =>  {
              domino.high + domino.low + z
            }}
            .toFloat
          }
        }
        val beforeRobotScore = beforeGame.playerByName("robot").score.toFloat
        val beforeHumanScore = beforeGame.playerByName("human").score.toFloat
        val lastTurnScore = (isRobotTurn, (afterGame.board.score() % 5 == 0)) match {
          case (true, true) => afterGame.board.score()
          case (false, true) => (-1)*afterGame.board.score()
          case (_, false) => 0.0
        }
        beforeRobotScore - beforeHumanScore + turnBonus + tileBonus + lastTurnScore
      }
      case (false, _) => {
        // Mid-game.
        val forcedDominoes: Boolean = if (perspective.hand.length != 1) {
            false
          } else {
            val movesWithoutAnswers: List[Move] = Domino.all()
              .flatMap( (domino) => {
                Direction.all.flatMap( (direction) => {
                  if (
                    perspective.hand.contains(domino) ||
                    afterGame.board.contains(domino)
                  ) {
                    List()
                  } else {
                    val move = Move(domino, direction)
                    val future: Option[Game] = afterGame.forceMove(move)
                    future.map((g) => {
                      val hasAnswer = perspective.hand.exists((d) => {
                        Direction.all.exists((dir) => {
                          val m = Move(d, dir)
                          !g.forceMove(m).isEmpty
                        })
                      })
                      if (hasAnswer) List() else List(move)
                    }).getOrElse(List())
                  }
                })
              })
            movesWithoutAnswers.isEmpty
          }
        val fd = (if (forcedDominoes) 10.0 else 0.0)*(if (perspective.name == "robot") 1.0 else -1.0)
        // val scoringPotential = afterGame.scoringMoves
        //   .filter{ case (move,_) => opponentHand.contains(move.domino) }
        //   .map{ case (_, score) => score }
        //   .sum
        robotScore - humanScore + fd
      }
    }
  }

  /**
   * Score a game after applying the given action. Works primarily by averaging
   * the values from multiple simluations.
   *
   * 1) If there is no reason to look farther into the future, either because the
   *    game is over or one player got dominoes, we use the heuristic.
   *
   * 2) If we reached our max depth, use the heuristic.
   *
   * 3) If the action is a draw, we need to consider all possible draw tiles.
   *    For each one, create a new MaxNode with that tile and return the average
   *    of the score of those MaxNodes.
   *
   * 4) If the action is a move or pass, play the best opponent move and then reconsider the position.
   *    In order to do this concretely, simulate the opponent's hand and for each hand,
   *    create a Min/MaxNode with that hand. Then, consider that position from the opponent's perspective
   *    in order to find its best move. Once the best opponent move is found, play it and convert that node
   *    into a Max/MinNode.
   */
  def score(): Double = {
    if (
      afterGame.isOver ||
      afterGame.playerOne.hand.isEmpty ||
      afterGame.playerTwo.hand.isEmpty ||
      (depthRemaining <= 0)
    ) {
      heuristic()
    } else {
      recursiveScore()
    }
  }

  def recursiveScore(): Double = {
    val isRobotTurn = (afterGame.activePlayer().name == "robot")
    val opponentHands = if (perspective.maybeOpponentHand.isEmpty) {
      (1 to numSimulations)
        .map((_) => perspective.opponentHand.getSample())
        .distinct
    } else {
      List(perspective.maybeOpponentHand.get)
    }
    val opponentSum = opponentHands
      .map((hand) => {
        if (action == Draw) {
          val updatedPerspective = perspective
            .assumeOpponentHand(hand,beforeGame.board.tiles())
          (0 to 5).map((t) => {
            // perspective update will choose a random tile
            if (isRobotTurn) {
              MaxNode(
                updatedPerspective.update(beforeGame, afterGame),
                afterGame,
                depthRemaining,
                numSimulations,
              ).toMinNode().map(_.score()).getOrElse(0.0)
            } else {
              MinNode(
                updatedPerspective.update(beforeGame, afterGame),
                afterGame,
                depthRemaining,
                numSimulations,
              ).toMaxNode().map(_.score()).getOrElse(0.0)
            }
          })
        } else {
          val safeGame = afterGame.withReplacedActiveHand(hand)
          val updatedPerspective = perspective.assumeOpponentHand(hand,beforeGame.board.tiles()).update(beforeGame, afterGame)
          if (isRobotTurn) {
            List(MaxNode(
              updatedPerspective.swap(),
              safeGame,
              depthRemaining,
              numSimulations,
            ).toMinNode().map(_.score()).getOrElse(0.0))
          } else {
            List(MinNode(
              updatedPerspective.swap(),
              safeGame,
              depthRemaining,
              numSimulations,
            ).toMaxNode().map(_.score()).getOrElse(0.0))
          }
        }
      })
      .foldRight[Double](0.0)( (xs, z) => {
        z + xs.sum/xs.length
      })
    opponentSum/opponentHands.length.toFloat
  }
}
