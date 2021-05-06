package game

import org.scalatest.flatspec.AnyFlatSpec

class GameSpec extends AnyFlatSpec {

  "A Game" should "be setup" in {
    val x = Game.setup("a", "b")
    assert(x.board == Board.empty)
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
        Some(PlayedDomino(Domino(6,6),12)),
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
      GameLog.empty,
      false,
      false,
    )
    assert(x.board.tiles == List(Domino(6,6)))
    val expectedLegalTilesToPlay = List(
      Domino(6,5),
      Domino(6,4),
      Domino(6,3),
      Domino(6,2),
      Domino(6,1),
      Domino(6,0),
    ).toSet
    assert(x.legalTilesToPlay.toSet == expectedLegalTilesToPlay)
  }


  "A Game" should "know the legal tiles to play: spinner and left" in {
    val x = Game(
      Board(
        Some(PlayedDomino(Domino(6,6),12)),
        List(PlayedDomino(Domino(6,3),3)),
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
      GameLog.empty,
      false,
      false,
    )
    assert(x.board.tiles == List(Domino(6,6), Domino(6,3)))
    val expectedLegalTilesToPlay = List(
      Domino(6,5),
      Domino(6,4),
      Domino(6,2),
      Domino(6,1),
      Domino(6,0),
      Domino(5,3),
      Domino(4,3),
      Domino(3,3),
      Domino(3,2),
      Domino(3,1),
      Domino(3,0),
    ).toSet
    assert(x.legalTilesToPlay.toSet == expectedLegalTilesToPlay)
  }

  "A Game" should "know the legal tiles to play: spinner, left, and right" in {
    val x = Game(
      Board(
        Some(PlayedDomino(Domino(6,6),12)),
        List(PlayedDomino(Domino(6,1),1)),
        List(PlayedDomino(Domino(6,4),4)),
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
      GameLog.empty,
      false,
      false,
    )
    val expectedLegalTilesToPlay = List(
      Domino(6,5),
      Domino(6,3),
      Domino(6,2),
      Domino(6,0),
      Domino(5,4),
      Domino(4,4),
      Domino(4,3),
      Domino(4,2),
      Domino(4,1),
      Domino(4,0),
      Domino(5,1),
      Domino(4,1),
      Domino(3,1),
      Domino(2,1),
      Domino(1,1),
      Domino(1,0),
    ).toSet
    assert(x.legalTilesToPlay.toSet == expectedLegalTilesToPlay)
  }

  "A Game" should "know the legal tiles to play: snake" in {
    val x = Game(
      Board(
        None,
        List(PlayedDomino(Domino(6,4),6), PlayedDomino(Domino(4,3),4)),
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
      GameLog.empty,
      false,
      false,
    )
    assert(x.board.tiles == List(Domino(6,4), Domino(4,3)))
    val expectedLegalTilesToPlay = List(
      Domino(6,6),
      Domino(6,5),
      Domino(6,3),
      Domino(6,2),
      Domino(6,1),
      Domino(6,0),
      Domino(5,3),
      Domino(3,3),
      Domino(3,2),
      Domino(3,1),
      Domino(3,0),
    ).toSet
    assert(x.legalTilesToPlay.toSet == expectedLegalTilesToPlay)
  }
}
