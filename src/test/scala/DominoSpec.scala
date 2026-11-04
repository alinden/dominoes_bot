package game

import org.scalatest.flatspec.AnyFlatSpec

class DominoSpec extends AnyFlatSpec {

  "A Domino" should "be created" in {
    val x = Domino(3, 3)
    assert(x.low == 3)
    assert(x.high == 3)
  }

  "A Domino" should "print nicely" in {
    assert(Domino(4, 3).toString() == "[4,3]")
  }

  "A non-double Domino" should "not think it is a double" in {
    assert(Domino(4, 3).isDouble() == false)
  }

  "A double Domino" should "know it is a double" in {
    assert(Domino(4, 4).isDouble() == true)
  }

  "A Domino" should "generate all 28 unique tiles" in {
    val tiles = Domino.all()

    assert(tiles.length == 28)
    assert(tiles.distinct.length == 28)
    assert(tiles.forall(d => d.high >= d.low))
    assert(
      tiles.forall(d =>
        d.high >= 0 && d.high <= 6 &&
          d.low >= 0 && d.low <= 6,
      ),
    )
  }

  "A Domino" should "generate all seven doubles" in {
    val doubles = Domino.doubles()

    assert(doubles.length == 7)
    assert(doubles.toSet == (0 to 6).map(n => Domino(n, n)).toSet)
  }

  "A Domino" should "identify exactly the generated doubles" in {
    val doubles = Domino.all().filter(_.isDouble())

    assert(doubles.toSet == Domino.doubles().toSet)
  }
}
