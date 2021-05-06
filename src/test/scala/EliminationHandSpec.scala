package game

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec

class EliminationHandSpec extends AnyFlatSpec {
  "An EliminationHand" should "be created with size 7 by default" in {
    val x = EliminationHand.createFromOpponentHand(
        "human",
        List(
          Domino(6,6),
          Domino(6,5),
          Domino(6,4),
          Domino(6,3),
          Domino(5,5),
          Domino(5,4),
          Domino(4,4),
      )
    )
    assert(x.playerName == "human")
    assert(x.length == 7)
    assert(!x.contains(Domino(6,6)))
    assert(x.likelihood(Domino(6,6)) == 0.0)
    assert(((x.likelihood(Domino(0,0))*1000).toInt) == 333)
    assert(x.possibilities.length == 116280)
  }

  "An EliminationHand" should "be created with a small size" in {
    // Hand is either the [5,1] or the [4,1] with equal likelihood.
    val possibleTiles = Set(Domino(5,1), Domino(4,1))
    val x = EliminationHand.createFromOpponentHand(
        "human",
        Domino.all.filterNot((d) => possibleTiles.contains(d)),
        1,
    )
    assert(x.length == 1)
    assert(x.contains(Domino(5,1)))
    assert(x.contains(Domino(4,1)))
    assert(x.likelihood(Domino(5,1)) == 0.5)
    assert(x.likelihood(Domino(4,1)) == 0.5)
    assert(x.possibilities.length == 2)
  }

  "An EliminationHand" should "be updated for an opponent play" in {
    // Hand is either the [5,1] or the [4,1] with equal likelihood.
    val possibleTiles = Set(Domino(5,1), Domino(4,1))
    val x = EliminationHand.createFromOpponentHand(
        "human",
        Domino.all.filterNot((d) => possibleTiles.contains(d)),
        1,
    ).updateForOpponentMove(Domino(5,1)).normalize
    assert(x.length == 1)
    assert(!x.contains(Domino(5,1)))
    assert(x.contains(Domino(4,1)))
    assert(x.likelihood(Domino(5,1)) == 0.0)
    assert(x.likelihood(Domino(4,1)) == 1.0)
    assert(x.possibilities.length == 1)
  }

  "An EliminationHand" should "be updated for a draw (late game)" in {
    // Hand is either the [5,1] or the [4,1] with equal likelihood.
    // Remaining boneyard tile is the other one.
    // After drawing, we should have the [[5,1], [4,1]].
    val possibleTiles = Set(Domino(5,1), Domino(4,1))
    val x = EliminationHand.createFromOpponentHand(
        "human",
        Domino.all.filterNot((d) => possibleTiles.contains(d)),
        1,
    ).updateForDraw(DrawLogEntry("human", 0, List())).normalize
    assert(x.likelihood(Domino(5,1)) == 1.0)
    assert(x.likelihood(Domino(4,1)) == 1.0)
    assert(x.getSample().sortBy(_.high).reverse == List(Domino(5,1), Domino(4,1)))
  }

  "An EliminationHand" should "be updated for a play (skip bayes)" in {
    // We have two tiles from the [1,4], [1,5] and [1,6].
    // Play the [1,4].
    // We should now have either the [1,5] or the [1,6].
    val remainingTiles = List(
      Domino(6,1),
      Domino(5,1),
      Domino(4,1),
    )
    val x = EliminationHand.createFromOpponentHand(
        "human",
        Domino.all.filterNot((d) => remainingTiles.contains(d)),
        2,
    ).updateForSelfMove(Move(Domino(4,1),Left), Game.setup("human", "robot"), true).normalize
    assert(x.length == 1)
    assert(x.possibilities.length == 2)
    assert((x.likelihood(Domino(5,1))*100).toInt == 50)
    assert((x.likelihood(Domino(6,1))*100).toInt == 50)
  }

  "An EliminationHand" should "be updated for an opponent highest double play" in {
    val x = EliminationHand.createFromOpponentHand(
        "human",
        List(
          Domino(6,6),
          Domino(6,5),
          Domino(6,4),
          Domino(6,3),
          Domino(5,5),
          Domino(5,4),
          Domino(4,4),
      )
    ).updateForHighestDouble(HighestDoubleEntry("robot", Domino(1,1))).normalize
    assert(x.length == 7)
    assert(x.possibilities.length == 31824)
    assert(!x.contains(Domino(3,3)))
    assert(!x.contains(Domino(2,2)))
    assert(!x.contains(Domino(1,1)))
    assert(x.contains(Domino(0,0)))
    assert(x.impossible.contains(Domino(1,1)))
    assert(!x.impossible.contains(Domino(2,2))) // could be drawn later
  }

  "An EliminationHand" should "be updated for a self highest double play" in {
    val x = EliminationHand.createFromOpponentHand(
        "human",
        List(
          Domino(6,6),
          Domino(6,5),
          Domino(6,4),
          Domino(6,3),
          Domino(5,5),
          Domino(5,4),
          Domino(4,4),
      )
    ).updateForHighestDouble(HighestDoubleEntry("human", Domino(1,1))).normalize
    assert(x.length == 6)
    assert(x.possibilities.length == 18564)
    assert(!x.contains(Domino(3,3)))
    assert(!x.contains(Domino(2,2)))
    assert(!x.contains(Domino(1,1)))
    assert(!x.contains(Domino(1,1)))
    assert(x.contains(Domino(0,0)))
    assert(x.impossible.contains(Domino(1,1)))
    assert(!x.impossible.contains(Domino(2,2))) // could be drawn later
  }

  "An EliminationHand" should "be updated for a draw (early game)" in {
    // If we don't have any 2s and we observe the opponent draw on
    // the [2,2], we should know they have (14 choose 7)*14 possible
    // hands.
    val twos = List(
      Domino(2,0),
      Domino(2,1),
      Domino(2,2),
      Domino(3,2),
      Domino(4,2),
      Domino(5,2),
      Domino(6,2),
    )
    val x = EliminationHand.createFromOpponentHand(
        "human",
        List(
          Domino(6,6),
          Domino(6,5),
          Domino(6,4),
          Domino(6,3),
          Domino(5,5),
          Domino(5,4),
          Domino(4,4),
      )
    ).updateForDraw(DrawLogEntry("human", 0, twos)).normalize
    assert(x.length == 8)
    assert(x.possibilities.length == 27027)
    assert(x.likelihood(Domino(2,0)) < x.likelihood(Domino(1,0)))
    assert((x.likelihood(Domino(2,0))*100).toInt == 7)
    assert((x.likelihood(Domino(1,0))*100).toInt == 53)
  }

  "An EliminationHand" should "get a sample" in {
    // Hand is either the [5,1] or the [4,1] with equal likelihood.
    val possibleTiles = Set(Domino(5,1), Domino(4,1))
    val x = EliminationHand.createFromOpponentHand(
        "human",
        Domino.all.filterNot((d) => possibleTiles.contains(d)),
        1,
    )
    val n = 10000
    val e = 0.01 // n*e == 100
    val samples: List[Map[List[Domino],Int]] = (0 to n).toList
      .map((_) => {
        Map(x.getSample() -> 1)
      })
    val sampleCounts: Map[List[Domino], Int] = samples.reduce(_ |+| _)
    assert(x.length == 1)
    assert(x.likelihood(Domino(5,1)) == 0.5)
    assert(x.likelihood(Domino(4,1)) == 0.5)
    assert(sampleCounts.get(List(Domino(5,1))).get <= (n*0.5 + n*e).toInt)
    assert(sampleCounts.get(List(Domino(5,1))).get >= (n*0.5 - n*e).toInt)
  }

  // ------------------------------------------------------------
  // Performance Tests
  "An EliminationHand" should "get many samples quickly" in {
    val x = EliminationHand.createFromOpponentHand(
        "human",
        List(
          Domino(6,6),
          Domino(6,5),
          Domino(6,4),
          Domino(6,3),
          Domino(5,5),
          Domino(5,4),
          Domino(4,4),
      )
    ).truncate.normalize
    val t0 = System.nanoTime()
    val samples = for {
      _ <- (0 to 50)
    } yield x.getSample()
    val t1 = System.nanoTime()
    val ms = (t1 - t0)/(1000*1000)
    assert(ms <= 200)
  }

  "An EliminationHand" should "update for an opponent play quickly" in {
    val x = EliminationHand.createFromOpponentHand(
        "human",
        List(
          Domino(6,6),
          Domino(6,5),
          Domino(6,4),
          Domino(6,3),
          Domino(5,5),
          Domino(5,4),
          Domino(4,4),
      )
    )
    val t0 = System.nanoTime()
    val updatedHands = for {
      _ <- (0 to 50)
    } yield x.updateForOpponentMove(Domino(5,1))
    val t1 = System.nanoTime()
    val ms = (t1 - t0)/(1000*1000)
    assert(ms <= 2000)
  }
}
