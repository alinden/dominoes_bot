package game

import org.scalatest.flatspec.AnyFlatSpec

class RobotSpec extends AnyFlatSpec {
  "A Robot" should "know the score at the move before dominoes" in {
    val robotHand = List(Domino(5,3))
    val game = Game(
      Board(
        Some(PlayedDomino(Domino(6,6),12)),
        List(
          PlayedDomino(Domino(5,4),5),
          PlayedDomino(Domino(4,3),4),
          PlayedDomino(Domino(3,1),3),
          PlayedDomino(Domino(6,1),1),
        ),
        List(
          PlayedDomino(Domino(4,2),2),
          PlayedDomino(Domino(4,4),4),
          PlayedDomino(Domino(6,4),4),
        ),
        List(
          PlayedDomino(Domino(5,5),5),
          PlayedDomino(Domino(5,2),5),
          PlayedDomino(Domino(2,0),2),
          PlayedDomino(Domino(6,0),0),
        ),
        List(),
      ),
      Player(
        "human",
        List(Domino(3,3), Domino(1,1)),
        45,
      ),
      Player(
        "robot",
        robotHand,
        60,
      ),
      PlayerTwoTurn,
      List(
        Domino(6,5),
        Domino(6,3),
        Domino(6,2),
        Domino(5,1),
        Domino(5,0),
        Domino(4,1),
        Domino(4,0),
        Domino(3,2),
        Domino(3,0),
        Domino(2,2),
        Domino(2,1),
        Domino(1,0),
        Domino(0,0),
      ),
      GameLog.empty,
      false,
      false,
    )
    val guess = EliminationHand(
      "human",
      List(
        PossibleHand(
          Set(Domino(3,3),Domino(1,1)),
          0.5,
          0.5,
        ), // 10 points * 50% = 5 points
        PossibleHand(
          Set(Domino(3,3),Domino(0,0)),
          0.25,
          0.75,
        ), // 5 points * 25% = 1.25 points
        PossibleHand(
          Set(Domino(0,0),Domino(1,1)),
          0.25,
          1.0,
        ), // 0 points * 25% = 0 points
      ), // => EV of adding this hand's tiles should be 6.25 points.
      Set(
        Domino(5,3),
        Domino(6,6),
        Domino(5,4),
        Domino(4,3),
        Domino(3,1),
        Domino(6,1),
        Domino(4,2),
        Domino(4,4),
        Domino(6,4),
        Domino(5,5),
        Domino(5,2),
        Domino(2,0),
        Domino(6,0),
      ),
    )
    val perspective = Perspective(
      "robot",
      "human",
      robotHand,
      None,
      guess,
      None,
    )
    val depth = 5 // not relevant
    val numSimulations = 30
    val maxNode = MaxNode(perspective, game, depth, numSimulations, true)
    val (score, action) = maxNode.bestAction()
    assert(action == Move(Domino(5,3),Left))
    // Robot is winning by 15 and will score 5 points with its last move.
    // Then it needs to add 6.25 for the opponent's tiles EV and
    // then 5 points for starting first on the next round.
    val expectedScore = 43
    assert(score < expectedScore + 1.5)
    assert(score > expectedScore - 1.5)
  }

  "A Robot" should "know the score at the move before a locked game" in {
    val robotHand = List(Domino(5,3),Domino(6,3))
    val game = Game(
      Board(
        Some(PlayedDomino(Domino(6,6),12)),
        List(
          PlayedDomino(Domino(6,5),6),
          PlayedDomino(Domino(5,4),5),
          PlayedDomino(Domino(4,3),4),
          PlayedDomino(Domino(3,1),3),
          PlayedDomino(Domino(6,1),1),
        ),
        List(
          PlayedDomino(Domino(6,0),6),
          PlayedDomino(Domino(1,0),0),
          PlayedDomino(Domino(2,1),1),
          PlayedDomino(Domino(5,2),2),
          PlayedDomino(Domino(5,0),0),
          PlayedDomino(Domino(2,0),0),
          PlayedDomino(Domino(4,2),2),
          PlayedDomino(Domino(4,4),4),
          PlayedDomino(Domino(6,4),4),
        ),
        List(
          PlayedDomino(Domino(3,2),3),
          PlayedDomino(Domino(6,2),2),
        ),
        List(),
      ),
      Player(
        "human",
        List(
          Domino(3,3),
          Domino(1,1),
          ),
        45,
      ),
      Player(
        "robot",
        robotHand,
        60,
      ),
      PlayerTwoTurn,
      List(
        Domino(5,5),
        Domino(5,1),
        Domino(4,1),
        Domino(4,0),
        Domino(3,0),
        Domino(2,2),
        Domino(0,0), // 32 points. Plus the human hand is 40.
      ),
      GameLog.empty,
      false,
      false,
    )
    val guess = EliminationHand(
      "human",
      List(
        PossibleHand(
          Set(Domino(3,3),Domino(1,1)),
          0.5,
          0.5,
        ),
        PossibleHand(
          Set(Domino(3,3),Domino(0,0)),
          0.25,
          0.75,
        ),
        PossibleHand(
          Set(Domino(0,0),Domino(1,1)),
          0.25,
          1.0,
        ),
      ),
      Set(
        Domino(6,6),
        Domino(6,5),
        Domino(5,4),
        Domino(4,3),
        Domino(3,1),
        Domino(6,1),
        Domino(6,0),
        Domino(1,0),
        Domino(2,1),
        Domino(5,2),
        Domino(5,0),
        Domino(2,0),
        Domino(4,2),
        Domino(4,4),
        Domino(6,4),
        Domino(3,2),
        Domino(6,2),
      )
    )
    val perspective = Perspective(
      "robot",
      "human",
      robotHand,
      None,
      guess,
      None,
    )
    val depth = 13
    val numSimulations = 30
    val maxNode = MaxNode(perspective, game, depth, numSimulations, true)
    val (score, action) = maxNode.bestAction()
    assert(action == Move(Domino(6,3),Up))
    // Robot is winning by 15 and will score 30 points with a locked
    // game.
    assert(score == 45)
  }
}
