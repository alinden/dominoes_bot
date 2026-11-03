package game

import org.scalatest.flatspec.AnyFlatSpec

class RobotSpec extends AnyFlatSpec {
  "A Robot" should "know the score at the move before dominoes" in {
    val gameState = parseGame(
      center = "[6,6]:12",
      left = "[5,4]:5 [4,3]:4 [3,1]:3 [6,1]:1",
      right = "[4,2]:2 [4,4]:4 [6,4]:4",
      up = "[5,5]:5 [5,2]:5 [2,0]:2 [6,0]:0",
      robot = "[5,3]",
      human = "[3,3] [1,1]",
      boneyard = "[6,5] [6,3] [6,2] [5,1] [5,0] [4,1] [4,0] " +
        "[3,2] [3,0] [2,2] [2,1] [1,0] [0,0]",
    )
    val perspective = robotPerspective(
      gameState,
      List(
        "[3,3] [1,1]" -> 0.50,
        "[3,3] [0,0]" -> 0.25,
        "[0,0] [1,1]" -> 0.25,
      ),
    )
    val depth = 13
    val numSimulations = 100
    val maxNode = MaxNode(perspective, gameState, depth, numSimulations, true)
    val (score, action) = maxNode.bestAction()
    assert(action == Move(Domino(5, 3), Left))
  }

  "A Robot" should "know the score at the move before a locked game" in {
    val gameState = parseGame(
      center = "[6,6]:12",
      left = "[6,5]:6 [5,5]:5 [5,1]:5 [4,1]:4 [4,3]:4 [3,1]:3 [6,1]:1",
      right = "[6,0]:6 [1,0]:0 [2,1]:1 [5,2]:2 [5,0]:0 " +
        "[2,0]:0 [4,2]:2 [4,4]:4 [6,4]:4",
      up = "[3,2]:3 [6,2]:2",
      robot = "[5,3] [6,3]",
      human = "[3,3] [1,1]",
      boneyard = "[5,4] [4,0] [3,0] [2,2] [0,0]",
    )
    val perspective = robotPerspective(
      gameState,
      List(
        "[3,3] [1,1]" -> 0.05,
        "[3,3] [0,0]" -> 0.05,
        "[0,3] [1,1]" -> 0.9, // assuming the opponent should have the [3,0] means we are unlikely to get 20 points after the opponent plays the [3,3]
      ),
    )
    val depth = 13
    val numSimulations = 100
    val maxNode = MaxNode(perspective, gameState, depth, numSimulations, true)

    val (score, action) = maxNode.bestAction()

    assert(action == Move(Domino(6, 3), Up))
    assert(math.abs(score.value - math.tanh(0.5)) < 0.000001)
  }

  private def dominoes(s: String): List[Domino] =
    """\[(\d),(\d)\]""".r
      .findAllMatchIn(s)
      .map(m => Domino(m.group(1).toInt, m.group(2).toInt))
      .toList

  private def played(s: String): List[PlayedDomino] =
    """\[(\d),(\d)\]:(\d+)""".r
      .findAllMatchIn(s)
      .map { m =>
        PlayedDomino(
          Domino(m.group(1).toInt, m.group(2).toInt),
          m.group(3).toInt,
        )
      }
      .toList

  private def parseGame(
      center: String,
      left: String = "",
      right: String = "",
      up: String = "",
      down: String = "",
      robot: String,
      human: String,
      boneyard: String,
      robotScore: Int = 60,
      humanScore: Int = 45,
      turn: Turn = PlayerTwoTurn,
  ): Game = {
    val centerDomino = dominoes(center).head

    Game(
      Board(
        Some(PlayedDomino(centerDomino, centerDomino.high * 2)),
        played(left),
        played(right),
        played(up),
        played(down),
      ),
      Player("human", dominoes(human), humanScore),
      Player("robot", dominoes(robot), robotScore),
      turn,
      dominoes(boneyard),
      GameLog.empty,
      false,
      false,
    )
  }

  private def robotPerspective(
      game: Game,
      possibleHands: List[(String, Double)],
  ): Perspective = {
    val hands = possibleHands
      .scanLeft((0.0, Option.empty[PossibleHand])) { case ((cumulative, _), (tiles, probability)) =>
        val next = cumulative + probability
        (next, Some(PossibleHand(dominoes(tiles).toSet, probability, next)))
      }
      .tail
      .flatMap(_._2)
      .toVector

    val robotHand = game.playerByName("robot").hand

    Perspective(
      "robot",
      "human",
      robotHand,
      None,
      EliminationHand(
        "human",
        hands,
        (game.board.tiles() ++ robotHand).toSet,
      ),
      None,
    )
  }
}
