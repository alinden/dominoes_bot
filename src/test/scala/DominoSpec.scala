package game

import org.scalatest.flatspec.AnyFlatSpec

class DominoSpec extends AnyFlatSpec {

  "A Domino" should "be created" in {
    val x = Domino(3,3)
    assert(x.low == 3)
    assert(x.high == 3)
  }

  "A Domino" should "print nicely" in {
    assert(Domino(4,3).toString() == "[4,3]")
  }

  "A non-double Domino" should "not think it is a double" in {
    assert(Domino(4,3).isDouble() == false)
  }

  "A double Domino" should "know it is a double" in {
    assert(Domino(4,4).isDouble() == true)
  }
}
