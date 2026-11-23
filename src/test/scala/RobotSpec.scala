package game

import org.scalatest.flatspec.AnyFlatSpec
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import java.nio.file.{Files, Paths}

class RobotSpec extends AnyFlatSpec {
  "A Robot" should "match the shared fixed-position C++ scores" in {
    val lines = Files.readString(Paths.get("cpp/tests/fixed_search.txt")).linesIterator.toList
    val tiles = lines.head.split(" ").toList.map(id => Domino.all()(id.toInt))
    val opening = Game(
      Board.empty(),
      Player("human", tiles.take(7), 0),
      Player("robot", tiles.slice(7, 14), 0),
      HighestDouble,
      tiles.drop(14),
      GameLog.empty(),
    )
    val game = opening.playHighestDouble(Domino(6, 6)).get._1
    val hand = game.playerOne.hand
    val perspective = robotPerspective(game, List(hand.mkString(" ") -> 1.0))
      .copy(maybeOpponentHand = Some(hand))

    for ((depth, rows) <- lines.tail.groupBy(_.takeWhile(_ != ' ').toInt)) {
      val scores = MaxNode(perspective, game, depth, 1).getScoredMoves()
      val expected = rows.map { row =>
        val Array(_, id, lead, runway) = row.split(" "): @unchecked
        (Move(Domino.all()(id.toInt), Left), math.tanh(lead.toDouble / runway.toDouble))
      }
      assert(scores.map(_._2) == expected.map(_._1))
      scores.zip(expected).foreach { case ((score, _), (_, expectedScore)) =>
        assert(math.abs(score.value - expectedScore) < 1e-12)
      }
    }
  }

  "A Robot" should "know the score at the move before dominoes" in {
    val gameState = parseGame(
      center = "[6,6]",
      left = "[5,4] [4,3] [3,1] [6,1]",
      right = "[4,2] [4,4] [6,4]",
      up = "[5,5] [5,2] [2,0] [6,0]",
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
      center = "[6,6]",
      left = "[6,5] [5,5] [5,1] [4,1] [4,3] [3,1] [6,1]",
      right = "[6,0] [1,0] [2,1] [5,2] [5,0] " +
        "[2,0] [4,2] [4,4] [6,4]",
      up = "[3,2] [6,2]",
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

  "A Robot" should "take an immediate scoring opportunity" in {
    val game = parseGame(
      center = "[6,6]",
      robot = "[6,3] [6,1]",
      human = "[5,4] [4,2]",
      boneyard = "[5,5] [4,4]",
    )

    val perspective = robotPerspective(
      game,
      List("[5,4] [4,2]" -> 1.0),
    )

    val maxNode = MaxNode(perspective, game, 1, 1, true)
    val (_, action) = maxNode.bestAction()

    assert(action == Move(Domino(6, 3), Left))
  }

  "A Robot" should "adjust its move based on opponent hand probabilities" in {
    val game = parseGame(
      center = "[0,0]",
      robot = "[5,0] [2,0]",
      human = "[5,5] [2,6]",
      boneyard = "[6,6] [5,4]",
    )

    def bestMove(probabilityOfDouble: Double): Action = {
      val perspective = robotPerspective(
        game,
        List(
          "[5,5] [2,6]" -> probabilityOfDouble,
          "[5,4] [2,6]" -> (1.0 - probabilityOfDouble),
        ),
      )

      val maxNode = MaxNode(perspective, game, 3, 100, true)
      maxNode.bestAction()._2
    }

    // When the opponent probably holds [5,5], avoid exposing 5.
    assert(bestMove(0.95) == Move(Domino(2, 0), Left))

    // When the opponent probably doesn't hold [5,5], take the 5 points.
    assert(bestMove(0.05) == Move(Domino(5, 0), Left))
  }

  "A Robot" should "choose a legal action with its random strategy" in {
    val game = parseGame(
      center = "[6,6]",
      robot = "[6,3] [6,1]",
      human = "[5,4] [4,2]",
      boneyard = "[5,5] [4,4]",
    )

    val legalActions = game.allLegalActions()

    for (_ <- 1 to 100) {
      val action = Robot.getRandomLegalAction(game)
      assert(action.exists(legalActions.contains))
    }
  }

  "A Robot" should "prefer a scoring move with its greedy strategy" in {
    val game = parseGame(
      center = "[6,6]",
      robot = "[6,3] [6,1]",
      human = "[5,4] [4,2]",
      boneyard = "[5,5] [4,4]",
    )

    val action = Robot.greedyMove(game)

    assert(action.contains(Move(Domino(6, 3), Left)))
  }

  "A Robot" should "fall back to greedy when its search deadline has already expired" in {
    val game = parseGame(
      center = "[6,6]",
      robot = "[6,3] [6,1]",
      human = "[5,4] [4,2]",
      boneyard = "[5,5] [4,4]",
    )
    val guess = robotPerspective(game, List("[5,4] [4,2]" -> 1.0)).opponentHand
    val deadline = Some(TurnDeadline.after(Duration.Zero))
    val task = TurnDeadline.withCurrent(deadline) {
      assert(Robot.act(game, Some(guess), debug = false).contains(Move(Domino(6, 3), Left)))
      Robot.getBestMove(game, guess, false, 1, 1)
    }
    // Run outside the deadline scope to verify that each deferred root task receives it.
    intercept[TurnDeadline.Expired.type] { task.runSyncUnsafe() }
    assert(TurnDeadline.current.isEmpty)
  }

  "A Robot" should "search with a sampled hand estimate after inference expires" in {
    val game = parseGame(
      center = "[6,6]",
      robot = "[6,3] [6,1]",
      human = "[5,4] [4,2]",
      boneyard = "[5,5] [4,4]",
    )
    val expired = Some(TurnDeadline.after(Duration.Zero))
    val inferred = TurnDeadline.withCurrent(expired) {
      TurnDeadline.attempt(Robot.sampleUnknownHand(game))
    }
    assert(inferred.isEmpty)

    val fallback = Robot.sampleUnknownHand(game)
    val known = (game.playerTwo.hand ++ game.board.tiles()).toSet
    assert(fallback.possibilities.nonEmpty)
    assert(fallback.possibilities.forall(hand => hand.hand.size == 2 && !hand.hand.exists(known)))

    val searchDeadline = Some(TurnDeadline.after(5.seconds))
    val (_, action) = TurnDeadline.withCurrent(searchDeadline) {
      Robot.getBestMove(game, fallback, false, 1, 1).runSyncUnsafe()
    }
    assert(game.allLegalActions().contains(action))
  }

  private def dominoes(s: String): List[Domino] =
    """\[(\d),(\d)\]""".r
      .findAllMatchIn(s)
      .map(m => Domino(m.group(1).toInt, m.group(2).toInt))
      .toList

  private def played(s: String, center: Domino): List[PlayedDomino] = {
    val tiles = dominoes(s)

    val initial = PlayedDomino(center, center.high)

    tiles.reverse
      .foldLeft(List(initial)) { (chain, tile) =>
        val exposed = chain.head.connectTo(tile).getOrElse {
          throw new IllegalArgumentException(
            s"Cannot connect $tile to ${chain.head.tile}",
          )
        }

        PlayedDomino(tile, exposed) :: chain
      }
      .dropRight(1)
  }

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
        played(left, centerDomino),
        played(right, centerDomino),
        played(up, centerDomino),
        played(down, centerDomino),
      ),
      Player("human", dominoes(human), humanScore),
      Player("robot", dominoes(robot), robotScore),
      turn,
      dominoes(boneyard),
      GameLog.empty(),
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
