package game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PerspectiveSpec extends AnyFlatSpec with Matchers {

  val myHand = List(Domino(6, 6), Domino(5, 5))
  val theirHand = List(Domino(4, 4), Domino(3, 3))

  val opponentBelief = EliminationHand(
    "Bob",
    Vector.empty,
    Set.empty,
  )

  def perspective = Perspective(
    name = "Alice",
    opponentName = "Bob",
    hand = myHand,
    maybeOpponentHand = None,
    opponentHand = opponentBelief,
    opponentIdeaOfMyHand = None,
  )

  "assumeOpponentHand" should "store the assumed hand and initialize the opponent's belief" in {
    val result = perspective.assumeOpponentHand(
      theirHand,
      Domino.all().filterNot((myHand ++ theirHand).contains),
    )

    result.maybeOpponentHand shouldBe Some(theirHand)
    result.opponentIdeaOfMyHand should not be empty

    result.name shouldBe "Alice"
    result.opponentName shouldBe "Bob"
    result.hand shouldBe myHand
    result.opponentHand shouldBe opponentBelief
  }

  it should "preserve an existing opponent belief" in {
    val existingBelief = EliminationHand(
      "Alice",
      Vector.empty,
      Set(Domino(0, 0)),
    )

    val original = perspective.copy(
      opponentIdeaOfMyHand = Some(existingBelief),
    )

    val result = original.assumeOpponentHand(
      theirHand,
      Nil,
    )

    result.maybeOpponentHand shouldBe Some(theirHand)
    result.opponentIdeaOfMyHand shouldBe Some(existingBelief)
  }

  "swap" should "exchange player identities, hands, and beliefs" in {
    val assumed = perspective.assumeOpponentHand(
      theirHand,
      Domino.all().filterNot((myHand ++ theirHand).contains),
    )

    val swapped = assumed.swap()

    swapped.name shouldBe "Bob"
    swapped.opponentName shouldBe "Alice"
    swapped.hand shouldBe theirHand
    swapped.maybeOpponentHand shouldBe Some(myHand)

    swapped.opponentHand shouldBe assumed.opponentIdeaOfMyHand.get
    swapped.opponentIdeaOfMyHand shouldBe Some(assumed.opponentHand)

    swapped.swap() shouldBe assumed
  }
}
