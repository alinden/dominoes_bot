package game

import cats.implicits._

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
  def heuristic(): Score = {
    val robotScore = afterGame.playerByName("robot").score.toFloat
    val humanScore = afterGame.playerByName("human").score.toFloat
    val isLockedGame = afterGame.playerOne.hand.isEmpty && afterGame.playerTwo.hand.isEmpty
    val isDominoes = !isLockedGame && (afterGame.playerOne.hand.isEmpty || afterGame.playerTwo.hand.isEmpty)
    val isRobotTurn = afterGame.activePlayer() == afterGame.playerByName("robot")
    (isDominoes, isLockedGame) match {
      case (_, true) => {
        // Locked game.
        val runway = math.max(0.0, 150.0 - math.max(robotScore, humanScore))
        if (runway == 0.0) {
          Score(robotScore - humanScore)
        } else {
          Score((robotScore - humanScore)/runway)
        }
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
        val robotScoreWithBonuses = beforeRobotScore + turnBonus + tileBonus + lastTurnScore
        val runway = math.max(0.0, 150.0 - math.max(robotScoreWithBonuses, beforeHumanScore))
        if (runway == 0.0) {
          Score(robotScoreWithBonuses - beforeHumanScore)
        } else {
          Score((robotScoreWithBonuses - beforeHumanScore)/runway)
        }
      }
      case (false, _) => {
        // Mid-game.
        val runway = math.max(0.0, 150.0 - math.max(robotScore, humanScore))
        if (runway == 0.0) {
          Score(robotScore - humanScore)
        } else {
          Score((robotScore - humanScore)/runway)
        }
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
  def score(): Score = {
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

  def recursiveScore(): Score = {
    val isRobotTurn = (afterGame.activePlayer().name == "robot")
    val opponentHands = if (perspective.maybeOpponentHand.isEmpty) {
      (1 to numSimulations)
        .map((_) => perspective.opponentHand.getSample())
    } else {
      List(perspective.maybeOpponentHand.get)
    }
    opponentHands
      .map((hand) => {
        if (action == Draw) {
          val updatedPerspective = perspective
            .assumeOpponentHand(hand,beforeGame.board.tiles())
          val numDraws = if (beforeGame.board.tiles().length <= 1) 3 else 10
          (0 to numDraws).map((t) => {
            // perspective update will choose a random tile
            if (isRobotTurn) {
              MaxNode(
                updatedPerspective.update(beforeGame, afterGame),
                afterGame,
                depthRemaining,
                numSimulations,
              ).toMinNode().map(_.score()).getOrElse(Score(0.0))
            } else {
              MinNode(
                updatedPerspective.update(beforeGame, afterGame),
                afterGame,
                depthRemaining,
                numSimulations,
              ).toMaxNode().map(_.score()).getOrElse(Score(0.0))
            }
          }).reduce(_ |+| _)
        } else {
          val safeGame = afterGame.withReplacedActiveHand(hand)
          val updatedPerspective = perspective.assumeOpponentHand(hand,beforeGame.board.tiles()).update(beforeGame, afterGame)
          if (isRobotTurn) {
            MaxNode(
              updatedPerspective.swap(),
              safeGame,
              depthRemaining,
              numSimulations,
            ).toMinNode().map(_.score()).getOrElse(Score(0.0))
          } else {
            MinNode(
              updatedPerspective.swap(),
              safeGame,
              depthRemaining,
              numSimulations,
            ).toMaxNode().map(_.score()).getOrElse(Score(0.0))
          }
        }
      })
      .reduce(_ |+| _)
  }
}
