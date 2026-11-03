package game

import org.scalatest.flatspec.AnyFlatSpec

class PossibleHandSpec extends AnyFlatSpec {

  val a = Domino(6, 6)
  val b = Domino(5, 3)
  val c = Domino(0, 0)

  "A PossibleHand" should "construct from a set of dominoes" in {
    val tiles = Set(a, b)
    val hand = PossibleHand(tiles, 0.25, 0.5)

    assert(hand.hand == tiles)
    assert(hand.contains(a))
    assert(hand.contains(b))
    assert(!hand.contains(c))
    assert(hand.likelihood == 0.25)
    assert(hand.cumulativeLikelihood == 0.5)
  }

  it should "add a tile without modifying the original hand" in {
    val original = PossibleHand(Set(a, b), 0.25, 0.5)
    val updated = original.addTile(c)

    assert(updated.hand == Set(a, b, c))
    assert(original.hand == Set(a, b))
    assert(updated.likelihood == original.likelihood)
    assert(updated.cumulativeLikelihood == original.cumulativeLikelihood)
  }

  it should "remove a tile without modifying the original hand" in {
    val original = PossibleHand(Set(a, b), 0.25, 0.5)
    val updated = original.removeTile(a)

    assert(updated.hand == Set(b))
    assert(original.hand == Set(a, b))
    assert(!updated.contains(a))
    assert(updated.likelihood == original.likelihood)
  }

  it should "multiply likelihood without changing the hand" in {
    val original = PossibleHand(Set(a, b), 0.25, 0.5)
    val updated = original.multiplyLikelihood(2.0)

    assert(updated.likelihood == 0.5)
    assert(updated.cumulativeLikelihood == 0.5)
    assert(updated.hand == original.hand)
    assert(original.likelihood == 0.25)
  }

  it should "update cumulative likelihood independently" in {
    val original = PossibleHand(Set(a, b), 0.25, 0.5)
    val updated = original.setCumulativeLikelihood(0.75)

    assert(updated.cumulativeLikelihood == 0.75)
    assert(updated.likelihood == 0.25)
    assert(updated.hand == original.hand)
  }
}
