package game

import org.scalatest.flatspec.AnyFlatSpec

class PlayedDominoSpec extends AnyFlatSpec {

  "A PlayedDomino" should "be created" in {
    val x = PlayedDomino(Domino(3, 3), 6)
    assert(x.tile.low == 3)
    assert(x.tile.high == 3)
    assert(x.value == 6)
  }

  "A PlayedDomino" should "calculate its exposed score" in {
    val normal = PlayedDomino(Domino(6, 3), 3)
    val double = PlayedDomino(Domino(4, 4), 4)

    assert(normal.score == 3)
    assert(double.score == 8)
  }

  "A PlayedDomino" should "connect matching dominoes in either orientation" in {
    val played = PlayedDomino(Domino(6, 3), 3)

    assert(played.connectTo(Domino(5, 3)) == Some(5))
    assert(played.connectTo(Domino(3, 1)) == Some(1))
    assert(played.connectTo(Domino(6, 2)) == None)
  }

  "A PlayedDomino" should "connect tiles to doubles" in {
    val played = PlayedDomino(Domino(4, 4), 4)

    assert(played.connectTo(Domino(6, 4)) == Some(6))
    assert(played.connectTo(Domino(4, 2)) == Some(2))
    assert(played.connectTo(Domino(3, 1)) == None)
  }

  "A PlayedDomino" should "reverse its exposed end" in {
    val played = PlayedDomino(Domino(6, 3), 3)

    val reversed = played.backwards()

    assert(reversed.tile == played.tile)
    assert(reversed.value == 6)
    assert(reversed.backwards() == played)

    // Reversing doesn't mutate the original.
    assert(played.value == 3)
  }
}
