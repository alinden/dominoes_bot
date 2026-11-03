package game

case class AvgNode(
    perspective: Perspective,
    beforeGame: Game,
    afterGame: Game,
    action: Action,
    depthRemaining: Int,
    numSimulations: Int,
) extends EvalNode {

  def heuristic(game: Game = afterGame): Score = {
    val robot = game.playerByName("robot")
    val human = game.playerByName("human")

    val dominoesWinner =
      if (robot.hand.isEmpty && human.hand.nonEmpty) {
        Some("robot")
      } else if (human.hand.isEmpty && robot.hand.nonEmpty) {
        Some("human")
      } else {
        None
      }

    Heuristic.evaluate(
      robotScore = robot.score.toDouble,
      humanScore = human.score.toDouble,
      gameOver = game.isOver,
      dominoesWinner = dominoesWinner,
      mode = HeuristicMode.BoundedRunway,
    )
  }

  @annotation.tailrec
  private def drawUntilPlayable(
      game: Game,
      perspective: Perspective,
      draws: Int = 0,
  ): (Game, Perspective, Int) = {
    if (game.legalMoves().nonEmpty || game.boneyard.isEmpty) {
      (game, perspective, draws)
    } else {
      val nextGame = game.act(Draw, false, true).get
      val nextPerspective = perspective.update(game, nextGame)

      drawUntilPlayable(nextGame, nextPerspective, draws + 1)
    }
  }

  def score(): Score = {
    SearchStats.visit()

    if (
      depthRemaining <= 0 &&
      !afterGame.isOver &&
      afterGame.playerOne.hand.nonEmpty &&
      afterGame.playerTwo.hand.nonEmpty
    ) {
      heuristic()
    } else {
      recursiveScore()
    }
  }

  private def opponentHands(): Seq[List[Domino]] =
    perspective.maybeOpponentHand match {
      case Some(hand) => List(hand)
      case None =>
        (0 until numSimulations)
          .map(_ => perspective.opponentHand.getSample())
    }

  private def evaluateHand(hand: List[Domino]): Score = {
    if (
      afterGame.isOver ||
      afterGame.playerOne.hand.isEmpty ||
      afterGame.playerTwo.hand.isEmpty
    ) {
      evaluateTerminal(hand)
    } else if (action == Draw) {
      evaluateDraw(hand)
    } else {
      evaluateMove(hand)
    }
  }

  private def evaluateMove(hand: List[Domino]): Score = {
    val safeGame = afterGame.withReplacedActiveHand(hand)

    val updatedPerspective = perspective
      .assumeOpponentHand(hand, beforeGame.board.tiles())
      .update(beforeGame, afterGame)

    if (afterGame.activePlayer().name == "robot") {
      MaxNode(
        updatedPerspective.swap(),
        safeGame,
        depthRemaining,
        numSimulations,
      ).score()
    } else {
      MinNode(
        updatedPerspective.swap(),
        safeGame,
        depthRemaining,
        numSimulations,
      ).score()
    }
  }

  private def evaluateDraw(hand: List[Domino]): Score = {
    val updatedPerspective =
      perspective.assumeOpponentHand(hand, beforeGame.board.tiles())

    val numDraws =
      if (beforeGame.board.tiles().length <= 1) 3 else 10

    SearchStats.visitDraw(numDraws)

    ScoreAccumulator.average((0 until numDraws).map { _ =>
      val (readyGame, readyPerspective, _) =
        if (numDraws == 1) {
          val firstPerspective =
            updatedPerspective.update(beforeGame, afterGame)

          drawUntilPlayable(afterGame, firstPerspective)
        } else {
          val activeHand = updatedPerspective.hand
          val inactiveHand = hand
          val boardTiles = beforeGame.board.tiles()

          val availableTiles = Domino.all().filterNot { d =>
            boardTiles.contains(d) ||
            activeHand.contains(d) ||
            inactiveHand.contains(d)
          }

          val sampledGame = beforeGame.copy(
            playerOne = beforeGame.playerOne.copy(
              hand =
                if (beforeGame.playerOne.name == updatedPerspective.name)
                  activeHand
                else
                  inactiveHand,
            ),
            playerTwo = beforeGame.playerTwo.copy(
              hand =
                if (beforeGame.playerTwo.name == updatedPerspective.name)
                  activeHand
                else
                  inactiveHand,
            ),
            boneyard = scala.util.Random.shuffle(availableTiles),
          )

          drawUntilPlayable(sampledGame, updatedPerspective)
        }

      if (afterGame.activePlayer().name == "robot") {
        MaxNode(
          readyPerspective,
          readyGame,
          depthRemaining,
          numSimulations,
        ).bestAction()._1
      } else {
        MinNode(
          readyPerspective,
          readyGame,
          depthRemaining,
          numSimulations,
        ).bestAction()._1
      }
    })
  }

  private def evaluateTerminal(hand: List[Domino]): Score = {
    val sampledGame = beforeGame.copy(
      playerOne =
        if (beforeGame.playerOne.name == perspective.opponentName)
          beforeGame.playerOne.copy(hand = hand)
        else
          beforeGame.playerOne,
      playerTwo =
        if (beforeGame.playerTwo.name == perspective.opponentName)
          beforeGame.playerTwo.copy(hand = hand)
        else
          beforeGame.playerTwo,
    )

    val result = sampledGame.act(action, false, true).get

    heuristic(result)
  }

  def recursiveScore(): Score =
    ScoreAccumulator.average(
      opponentHands().map(evaluateHand),
    )
}
