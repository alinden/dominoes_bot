package game

import org.scalatest.flatspec.AnyFlatSpec

class GameSpec extends AnyFlatSpec {

  "A Game" should "be setup" in {
    val x = Game.setup("a", "b")
    assert(x.board == Board.empty())
    assert(x.playerOne.name == "a")
    assert(x.playerOne.hand.length == 7)
    assert(x.playerTwo.name == "b")
    assert(x.playerTwo.hand.length == 7)
    assert(x.boneyard.length == 14)
    assert(x.playerOne.hand.toSet.intersect(x.playerTwo.hand.toSet).isEmpty)
    assert(x.playerOne.hand.toSet.intersect(x.boneyard.toSet).isEmpty)
    assert(x.playerTwo.hand.toSet.intersect(x.boneyard.toSet).isEmpty)
  }

  "A Game" should "know the legal tiles to play: spinner only" in {
    val x = Game(
      Board(
        Some(PlayedDomino(Domino(6, 6), 12)),
        List(),
        List(),
        List(),
        List(),
      ),
      Player(
        "human",
        List(), // pone hand should be irrelevant
        0,
      ),
      Player(
        "robot",
        List(), // ptwo hand should be irrelevant
        0,
      ),
      PlayerOneTurn,
      List(), // boneyard should be irrelevant
      GameLog.empty(),
      false,
      false,
    )
    assert(x.board.tiles() == List(Domino(6, 6)))
    val expectedLegalTilesToPlay = List(
      Domino(6, 5),
      Domino(6, 4),
      Domino(6, 3),
      Domino(6, 2),
      Domino(6, 1),
      Domino(6, 0),
    ).toSet
    assert(x.legalTilesToPlay().toSet == expectedLegalTilesToPlay)
  }

  "A Game" should "know the legal tiles to play: spinner and left" in {
    val x = Game(
      Board(
        Some(PlayedDomino(Domino(6, 6), 12)),
        List(PlayedDomino(Domino(6, 3), 3)),
        List(),
        List(),
        List(),
      ),
      Player(
        "human",
        List(), // pone hand should be irrelevant
        0,
      ),
      Player(
        "robot",
        List(), // ptwo hand should be irrelevant
        0,
      ),
      PlayerOneTurn,
      List(), // boneyard should be irrelevant
      GameLog.empty(),
      false,
      false,
    )
    assert(x.board.tiles() == List(Domino(6, 6), Domino(6, 3)))
    val expectedLegalTilesToPlay = List(
      Domino(6, 5),
      Domino(6, 4),
      Domino(6, 2),
      Domino(6, 1),
      Domino(6, 0),
      Domino(5, 3),
      Domino(4, 3),
      Domino(3, 3),
      Domino(3, 2),
      Domino(3, 1),
      Domino(3, 0),
    ).toSet
    assert(x.legalTilesToPlay().toSet == expectedLegalTilesToPlay)
  }

  "A Game" should "know the legal tiles to play: spinner, left, and right" in {
    val x = Game(
      Board(
        Some(PlayedDomino(Domino(6, 6), 12)),
        List(PlayedDomino(Domino(6, 1), 1)),
        List(PlayedDomino(Domino(6, 4), 4)),
        List(),
        List(),
      ),
      Player(
        "human",
        List(), // pone hand should be irrelevant
        0,
      ),
      Player(
        "robot",
        List(), // ptwo hand should be irrelevant
        0,
      ),
      PlayerOneTurn,
      List(), // boneyard should be irrelevant
      GameLog.empty(),
      false,
      false,
    )
    val expectedLegalTilesToPlay = List(
      Domino(6, 5),
      Domino(6, 3),
      Domino(6, 2),
      Domino(6, 0),
      Domino(5, 4),
      Domino(4, 4),
      Domino(4, 3),
      Domino(4, 2),
      Domino(4, 1),
      Domino(4, 0),
      Domino(5, 1),
      Domino(4, 1),
      Domino(3, 1),
      Domino(2, 1),
      Domino(1, 1),
      Domino(1, 0),
    ).toSet
    assert(x.legalTilesToPlay().toSet == expectedLegalTilesToPlay)
  }

  "A Game" should "know the legal tiles to play: snake" in {
    val x = Game(
      Board(
        None,
        List(PlayedDomino(Domino(6, 4), 6), PlayedDomino(Domino(4, 3), 4)),
        List(),
        List(),
        List(),
      ),
      Player(
        "human",
        List(), // pone hand should be irrelevant
        0,
      ),
      Player(
        "robot",
        List(), // ptwo hand should be irrelevant
        0,
      ),
      PlayerOneTurn,
      List(), // boneyard should be irrelevant
      GameLog.empty(),
      false,
      false,
    )
    assert(x.board.tiles() == List(Domino(6, 4), Domino(4, 3)))
    val expectedLegalTilesToPlay = List(
      Domino(6, 6),
      Domino(6, 5),
      Domino(6, 3),
      Domino(6, 2),
      Domino(6, 1),
      Domino(6, 0),
      Domino(5, 3),
      Domino(3, 3),
      Domino(3, 2),
      Domino(3, 1),
      Domino(3, 0),
    ).toSet
    assert(x.legalTilesToPlay().toSet == expectedLegalTilesToPlay)
  }

  "A Game" should "simplify moves: snake" in {
    val x = Game(
      Board(
        None,
        List(
          PlayedDomino(Domino(6, 4), 6),
          PlayedDomino(Domino(4, 3), 4),
          PlayedDomino(Domino(3, 6), 6),
        ),
        List(),
        List(),
        List(),
      ),
      Player(
        "human",
        List(), // pone hand should be irrelevant
        0,
      ),
      Player(
        "robot",
        List(Domino(6, 6)),
        0,
      ),
      PlayerTwoTurn,
      List(), // boneyard should be irrelevant
      GameLog.empty(),
      false,
      false,
    )
    val simplifiedMoves = x.allLegalActions()
    val expectedSimplifiedMoves = List(
      Move(Domino(6, 6), Left),
    )
    assert(simplifiedMoves.toSet == expectedSimplifiedMoves.toSet)
  }

  "A Game" should "simplify moves: spinner" in {
    val x = Game(
      Board(
        Some(PlayedDomino(Domino(6, 6), 6)),
        List(),
        List(),
        List(),
        List(),
      ),
      Player(
        "human",
        List(), // pone hand should be irrelevant
        0,
      ),
      Player(
        "robot",
        List(Domino(6, 3)),
        0,
      ),
      PlayerTwoTurn,
      List(), // boneyard should be irrelevant
      GameLog.empty(),
      false,
      false,
    )
    val simplifiedMoves = x.allLegalActions()
    val expectedSimplifiedMoves = List(
      Move(Domino(6, 3), Left),
    )
    assert(simplifiedMoves.toSet == expectedSimplifiedMoves.toSet)
  }

  "A Game" should "simplify moves: spinner with some chains" in {
    val x = Game(
      Board(
        Some(PlayedDomino(Domino(6, 6), 6)),
        List(PlayedDomino(Domino(6, 4), 4)),
        List(PlayedDomino(Domino(6, 2), 2)),
        List(),
        List(),
      ),
      Player(
        "human",
        List(), // pone hand should be irrelevant
        0,
      ),
      Player(
        "robot",
        List(Domino(6, 3)),
        0,
      ),
      PlayerTwoTurn,
      List(), // boneyard should be irrelevant
      GameLog.empty(),
      false,
      false,
    )
    val simplifiedMoves = x.allLegalActions()
    val expectedSimplifiedMoves = List(
      Move(Domino(6, 3), Up),
    )
    assert(simplifiedMoves.toSet == expectedSimplifiedMoves.toSet)
  }

  "A Game" should "simplify moves: spinner with equal value chains" in {
    val x = Game(
      Board(
        Some(PlayedDomino(Domino(6, 6), 6)),
        List(
          PlayedDomino(Domino(4, 2), 2),
          PlayedDomino(Domino(6, 4), 4),
        ),
        List(PlayedDomino(Domino(6, 2), 2)),
        List(),
        List(),
      ),
      Player(
        "human",
        List(), // pone hand should be irrelevant
        0,
      ),
      Player(
        "robot",
        List(Domino(2, 2)),
        0,
      ),
      PlayerTwoTurn,
      List(), // boneyard should be irrelevant
      GameLog.empty(),
      false,
      false,
    )
    val simplifiedMoves = x.allLegalActions()
    val expectedSimplifiedMoves = List(
      Move(Domino(2, 2), Left),
    )
    assert(simplifiedMoves.toSet == expectedSimplifiedMoves.toSet)
  }

  "A Game" should "update the board, hand, score, and turn after a move" in {
    val tile = Domino(6, 3)
    val otherTile = Domino(2, 1)

    val game = Game(
      Board.empty().addFirstTile(Domino(6, 6)),
      Player("a", List(tile, otherTile), 10),
      Player("b", List(Domino(4, 2)), 20),
      PlayerOneTurn,
      List(),
      GameLog.empty(),
    )

    val updated = game.move(tile, Left).get

    assert(updated.board.contains(tile))
    assert(updated.playerOne.hand == List(otherTile))
    assert(updated.playerOne.score == 25) // 10 + 12 + 3
    assert(updated.playerTwo.score == 20)
    assert(updated.turn == PlayerTwoTurn)

    // Original game is unchanged.
    assert(game.playerOne.hand.contains(tile))
    assert(!game.board.contains(tile))
  }

  "A Game" should "reject illegal moves" in {
    val game = Game(
      Board.empty().addFirstTile(Domino(6, 6)),
      Player("a", List(Domino(6, 3)), 0),
      Player("b", List(), 0),
      PlayerOneTurn,
      List(),
      GameLog.empty(),
    )

    // Tile does not connect to the board.
    assert(game.move(Domino(5, 2), Left).isEmpty)

    // Tile connects, but the player doesn't own it.
    assert(game.move(Domino(6, 4), Left).isEmpty)
  }

  "A Game" should "treat directions as equivalent on a spinner-only board" in {
    val game = Game(
      Board.empty().addFirstTile(Domino(6, 6)),
      Player("a", List(Domino(6, 3)), 0),
      Player("b", List(), 0),
      PlayerOneTurn,
      List(),
      GameLog.empty(),
    )

    val left = game.move(Domino(6, 3), Left).get
    val up = game.move(Domino(6, 3), Up).get

    assert(left.board == up.board)
    assert(left.board.left.nonEmpty)
    assert(left.board.up.isEmpty)
  }

  "A Game" should "draw a tile when no moves are available" in {
    val drawnTile = Domino(6, 2)

    val game = Game(
      Board.empty().addFirstTile(Domino(6, 6)),
      Player("a", List(Domino(4, 3)), 0),
      Player("b", List(Domino(5, 1)), 0),
      PlayerOneTurn,
      List(drawnTile),
      GameLog.empty(),
    )

    val updated = game.draw().get

    assert(updated.playerOne.hand.contains(drawnTile))
    assert(updated.playerOne.hand.length == 2)
    assert(updated.boneyard.isEmpty)
    assert(updated.turn == PlayerOneTurn)
    assert(updated.board == game.board)
  }

  "A Game" should "reject drawing when a move is available" in {
    val game = Game(
      Board.empty().addFirstTile(Domino(6, 6)),
      Player("a", List(Domino(6, 3)), 0),
      Player("b", List(), 0),
      PlayerOneTurn,
      List(Domino(4, 2)),
      GameLog.empty(),
    )

    assert(game.draw().isEmpty)
  }

  "A Game" should "allow passing only when no moves or draws are available" in {
    val game = Game(
      Board.empty().addFirstTile(Domino(6, 6)),
      Player("a", List(Domino(4, 3)), 0),
      Player("b", List(Domino(5, 1)), 0),
      PlayerOneTurn,
      List(),
      GameLog.empty(),
    )

    val updated = game.pass().get

    assert(updated.turn == PlayerTwoTurn)
    assert(updated.lastTurnPass)
    assert(updated.board == game.board)
    assert(updated.playerOne == game.playerOne)
    assert(updated.playerTwo == game.playerTwo)

    // Cannot pass while drawing is possible.
    assert(game.copy(boneyard = List(Domino(2, 1))).pass().isEmpty)

    // Cannot pass while a legal move exists.
    assert(
      game.copy(playerOne = Player("a", List(Domino(6, 3)), 0))
        .pass()
        .isEmpty
    )
  }

  "A Game" should "end the round when a player plays their last tile" in {
    val tile = Domino(6, 3)

    val game = Game(
      Board.empty().addFirstTile(Domino(6, 6)),
      Player("a", List(tile), 20),
      Player("b", List(Domino(4, 3), Domino(2, 1)), 30),
      PlayerOneTurn,
      List(),
      GameLog.empty(),
    )

    val updated = game.act(Move(tile, Left), debug = false, dontShuffle = true).get

    assert(updated.playerOne.hand.isEmpty)
    assert(updated.playerOne.score == 45) // 20 + 15 for move + 10 for remaining pips
    assert(updated.playerTwo.score == 30)
    assert(updated.turn == PlayerOneTurn)
    assert(!updated.isOver)
  }

  "A Game" should "end a locked round after two consecutive passes" in {
    val game = Game(
      Board.empty().addFirstTile(Domino(6, 6)),
      Player("a", List(Domino(4, 3)), 20), // 7 pips
      Player("b", List(Domino(5, 3), Domino(5, 4)), 30), // 17 pips
      PlayerOneTurn,
      List(),
      GameLog.empty(),
    )

    val afterFirstPass = game.act(Pass, debug = false, dontShuffle = true).get
    val afterSecondPass =
      afterFirstPass.act(Pass, debug = false, dontShuffle = true).get

    assert(afterFirstPass.turn == PlayerTwoTurn)
    assert(afterFirstPass.lastTurnPass)

    assert(afterSecondPass.turn == HighestDouble)
    assert(afterSecondPass.playerOne.hand.isEmpty)
    assert(afterSecondPass.playerTwo.hand.isEmpty)
    assert(afterSecondPass.playerOne.score == 30) // 20 + 10
    assert(afterSecondPass.playerTwo.score == 30)
    assert(!afterSecondPass.isOver)
  }

  "A Game" should "start with the highest available double" in {
    val game = Game(
      Board.empty(),
      Player("a", List(Domino(4, 4), Domino(2, 1)), 10),
      Player("b", List(Domino(5, 5), Domino(3, 2)), 20),
      HighestDouble,
      List(),
      GameLog.empty(),
    )

    val updated = game.playHighestDouble().get

    assert(updated.board.contains(Domino(5, 5)))
    assert(!updated.playerTwo.hand.contains(Domino(5, 5)))
    assert(updated.playerTwo.score == 30)
    assert(updated.playerOne.score == 10)
    assert(updated.turn == PlayerOneTurn)
  }

  "A Game" should "end the game when a player reaches 150 points" in {
    val tile = Domino(6, 3)

    val game = Game(
      Board.empty().addFirstTile(Domino(6, 6)),
      Player("a", List(tile), 130),
      Player("b", List(Domino(4, 3), Domino(2, 1)), 100),
      PlayerOneTurn,
      List(),
      GameLog.empty(),
    )

    val updated = game.act(Move(tile, Left), debug = false, dontShuffle = true).get

    assert(updated.playerOne.score == 155)
    assert(updated.playerTwo.score == 100)
    assert(updated.isOver)
  }
}
