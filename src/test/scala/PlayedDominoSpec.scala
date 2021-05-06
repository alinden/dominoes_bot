package game

import org.scalatest.flatspec.AnyFlatSpec

class PlayedDominoSpec extends AnyFlatSpec {

  "A PlayedDomino" should "be created" in {
    val x = PlayedDomino(Domino(3,3), 6)
    assert(x.tile.low == 3)
    assert(x.tile.high == 3)
    assert(x.value == 6)
  }
}
