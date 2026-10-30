package game

import org.scalatest.flatspec.AnyFlatSpec

class MinMaxNodeSpec extends AnyFlatSpec {

  "MinNode.isBetterScore" should "accept moves within the score margin" in {
    val node = MinNode(null, null, 1, 1)
    val best = Score(0.05)

    // MinNode prefers lower scores.
    assert(node.isBetterScore(Score(0.06), best, 0.02))
    assert(!node.isBetterScore(Score(0.10), best, 0.02))
  }

  "MinNode" should "evaluate the risk of exposing a double" in {
    val observedMove = Move(Domino(6, 4), Left)

    val handWithDouble = List(
      Domino(5, 3),
      Domino(2, 2),
      Domino(6, 4),
      Domino(4, 4), // The [4,4] blocks the opponent from holding it and responding to our move with a 20 point score.
      Domino(6, 2),
      Domino(2, 0),
      Domino(6, 1),
    )

    val handWithoutDouble = List( // no [4,4] here
      Domino(5, 3),
      Domino(2, 2),
      Domino(6, 4),
      Domino(4, 3),
      Domino(6, 2),
      Domino(2, 0),
      Domino(6, 1),
    )

    def evaluate(hand: List[Domino], depth: Int) = {
      val board = Board(
        Some(PlayedDomino(Domino(6, 6), 12)),
        List(),
        List(),
        List(),
        List(),
      )

      val game = Game(
        board,
        Player("human", hand, 0),
        Player("robot", List(
          Domino(5, 5),
          Domino(5, 4),
          Domino(4, 2),
          Domino(3, 3),
          Domino(3, 2),
          Domino(1, 1),
        ), 0),
        PlayerOneTurn,
        Domino.all().filterNot(d =>
          hand.contains(d) ||
          d == Domino(6, 6) ||
          List(
            Domino(5, 5),
            Domino(5, 4),
            Domino(4, 2),
            Domino(3, 3),
            Domino(3, 2),
            Domino(1, 1),
          ).contains(d)
        ),
        GameLog.empty,
      )

      val perspective = Perspective(
        "human",
        "robot",
        hand,
        None,
        EliminationHand.createFromOpponentHand(
          "robot",
          hand ++ game.board.tiles(),
        ),
        None,
      )

      val node = MinNode(
        perspective,
        game,
        depth,
        1,
        true,
      )

      val scores = node.getScoredMoves()

      val observedScore = scores.collectFirst {
        case (score, action) if action == observedMove => score.value
      }.get

      val bestScore = scores.map(_._1.value).min

      (observedScore, bestScore, node.goodActions().contains(observedMove))
    }

    for (depth <- List(1, 2)) {
      val withDouble = evaluate(handWithDouble, depth)
      val withoutDouble = evaluate(handWithoutDouble, depth)

      info(s"Depth $depth")
      info(s"  With [4,4]:    $withDouble")
      info(s"  Without [4,4]: $withoutDouble")
    }
  }
}
