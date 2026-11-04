package game

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec

class EliminationHandSpec extends AnyFlatSpec {
  "An EliminationHand" should "be created with size 7 by default" in {
    val x = EliminationHand.createFromOpponentHand(
      "human",
      List(
        Domino(6, 6),
        Domino(6, 5),
        Domino(6, 4),
        Domino(6, 3),
        Domino(5, 5),
        Domino(5, 4),
        Domino(4, 4),
      ),
    )
    assert(x.playerName == "human")
    assert(x.length() == 7)
    assert(!x.contains(Domino(6, 6)))
    assert(x.likelihood(Domino(6, 6)) == 0.0)
    assert(((x.likelihood(Domino(0, 0)) * 1000).toInt) == 333)
    assert(x.possibilities.length == 116280)
  }

  "An EliminationHand" should "be created with a small size" in {
    // Hand is either the [5,1] or the [4,1] with equal likelihood.
    val possibleTiles = Set(Domino(5, 1), Domino(4, 1))
    val x = EliminationHand.createFromOpponentHand(
      "human",
      Domino.all().filterNot((d) => possibleTiles.contains(d)),
      1,
    )
    assert(x.length() == 1)
    assert(x.contains(Domino(5, 1)))
    assert(x.contains(Domino(4, 1)))
    assert(x.likelihood(Domino(5, 1)) == 0.5)
    assert(x.likelihood(Domino(4, 1)) == 0.5)
    assert(x.possibilities.length == 2)
  }

  "An EliminationHand" should "be updated for an opponent play" in {
    // Hand is either the [5,1] or the [4,1] with equal likelihood.
    val possibleTiles = Set(Domino(5, 1), Domino(4, 1))
    val x = EliminationHand
      .createFromOpponentHand(
        "human",
        Domino.all().filterNot((d) => possibleTiles.contains(d)),
        1,
      )
      .updateForOpponentMove(Domino(5, 1))
      .normalize()
    assert(x.length() == 1)
    assert(!x.contains(Domino(5, 1)))
    assert(x.contains(Domino(4, 1)))
    assert(x.likelihood(Domino(5, 1)) == 0.0)
    assert(x.likelihood(Domino(4, 1)) == 1.0)
    assert(x.possibilities.length == 1)
  }

  "An EliminationHand" should "be updated for a draw (late game)" in {
    // Hand is either the [5,1] or the [4,1] with equal likelihood.
    // Remaining boneyard tile is the other one.
    // After drawing, we should have the [[5,1], [4,1]].
    val possibleTiles = Set(Domino(5, 1), Domino(4, 1))
    val x = EliminationHand
      .createFromOpponentHand(
        "human",
        Domino.all().filterNot((d) => possibleTiles.contains(d)),
        1,
      )
      .updateForDraw(DrawLogEntry("human", 0, List()))
      .normalize()
    assert(x.likelihood(Domino(5, 1)) == 1.0)
    assert(x.likelihood(Domino(4, 1)) == 1.0)
    assert(x.getSample().sortBy(_.high).reverse == List(Domino(5, 1), Domino(4, 1)))
  }

  "An EliminationHand" should "be updated for a play (skip bayes)" in {
    // We have two tiles from the [1,4], [1,5] and [1,6].
    // Play the [1,4].
    // We should now have either the [1,5] or the [1,6].
    val remainingTiles = List(
      Domino(6, 1),
      Domino(5, 1),
      Domino(4, 1),
    )
    val x = EliminationHand
      .createFromOpponentHand(
        "human",
        Domino.all().filterNot((d) => remainingTiles.contains(d)),
        2,
      )
      .updateForSelfMove(Move(Domino(4, 1), Left), Game.setup("human", "robot"), true)
      .normalize()
    assert(x.length() == 1)
    assert(x.possibilities.length == 2)
    assert((x.likelihood(Domino(5, 1)) * 100).toInt == 50)
    assert((x.likelihood(Domino(6, 1)) * 100).toInt == 50)
  }

  "An EliminationHand" should "be updated for an opponent highest double play" in {
    val x = EliminationHand
      .createFromOpponentHand(
        "human",
        List(
          Domino(6, 6),
          Domino(6, 5),
          Domino(6, 4),
          Domino(6, 3),
          Domino(5, 5),
          Domino(5, 4),
          Domino(4, 4),
        ),
      )
      .updateForHighestDouble(HighestDoubleEntry("robot", Domino(1, 1)))
      .normalize()
    assert(x.length() == 7)
    assert(x.possibilities.length == 31824)
    assert(!x.contains(Domino(3, 3)))
    assert(!x.contains(Domino(2, 2)))
    assert(!x.contains(Domino(1, 1)))
    assert(x.contains(Domino(0, 0)))
    assert(x.impossible.contains(Domino(1, 1)))
    assert(!x.impossible.contains(Domino(2, 2))) // could be drawn later
  }

  "An EliminationHand" should "be updated for a self highest double play" in {
    val x = EliminationHand
      .createFromOpponentHand(
        "human",
        List(
          Domino(6, 6),
          Domino(6, 5),
          Domino(6, 4),
          Domino(6, 3),
          Domino(5, 5),
          Domino(5, 4),
          Domino(4, 4),
        ),
      )
      .updateForHighestDouble(HighestDoubleEntry("human", Domino(1, 1)))
      .normalize()
    assert(x.length() == 6)
    assert(x.possibilities.length == 18564)
    assert(!x.contains(Domino(3, 3)))
    assert(!x.contains(Domino(2, 2)))
    assert(!x.contains(Domino(1, 1)))
    assert(!x.contains(Domino(1, 1)))
    assert(x.contains(Domino(0, 0)))
    assert(x.impossible.contains(Domino(1, 1)))
    assert(!x.impossible.contains(Domino(2, 2))) // could be drawn later
  }

  "An EliminationHand" should "be updated for a draw (early game)" in {
    // If we don't have any 2s and we observe the opponent draw on
    // the [2,2], we should know they have (14 choose 7)*14 possible
    // hands.
    val twos = List(
      Domino(2, 0),
      Domino(2, 1),
      Domino(2, 2),
      Domino(3, 2),
      Domino(4, 2),
      Domino(5, 2),
      Domino(6, 2),
    )
    val x = EliminationHand
      .createFromOpponentHand(
        "human",
        List(
          Domino(6, 6),
          Domino(6, 5),
          Domino(6, 4),
          Domino(6, 3),
          Domino(5, 5),
          Domino(5, 4),
          Domino(4, 4),
        ),
      )
      .updateForDraw(DrawLogEntry("human", 0, twos))
      .normalize()
    assert(x.length() == 8)
    assert(x.possibilities.length == 27027)
    assert(x.likelihood(Domino(2, 0)) < x.likelihood(Domino(1, 0)))
    assert((x.likelihood(Domino(2, 0)) * 100).toInt == 7)
    assert((x.likelihood(Domino(1, 0)) * 100).toInt == 53)
  }

  "An EliminationHand" should "get a sample" in {
    // Hand is either the [5,1] or the [4,1] with equal likelihood.
    val possibleTiles = Set(Domino(5, 1), Domino(4, 1))
    val x = EliminationHand.createFromOpponentHand(
      "human",
      Domino.all().filterNot((d) => possibleTiles.contains(d)),
      1,
    )
    val n = 10000
    val e = 0.02 // 2% margin of error to avoid flaky tests
    val samples: List[Map[List[Domino], Int]] = (0 to n).toList
      .map((_) => {
        Map(x.getSample() -> 1)
      })
    val sampleCounts: Map[List[Domino], Int] = samples.reduce(_ |+| _)
    assert(x.length() == 1)
    assert(x.likelihood(Domino(5, 1)) == 0.5)
    assert(x.likelihood(Domino(4, 1)) == 0.5)
    assert(sampleCounts.get(List(Domino(5, 1))).get <= (n * 0.5 + n * e).toInt)
    assert(sampleCounts.get(List(Domino(5, 1))).get >= (n * 0.5 - n * e).toInt)
  }

  "An EliminationHand" should "adjust probabilities using tile weights" in {
    val a = Domino(5, 1)
    val b = Domino(4, 1)

    val hand = EliminationHand.createFromOpponentHand(
      "human",
      Domino.all().filterNot(Set(a, b).contains),
      1,
    )

    val updated = hand.applyTileWeights(
      Map(a -> 4.0, b -> 1.0),
      evidenceStrength = 0.5,
    )

    // sqrt(4) = 2, sqrt(1) = 1
    // Normalized probabilities: 2/3 and 1/3.
    assert(math.abs(updated.likelihood(a) - 2.0 / 3.0) < 1e-9)
    assert(math.abs(updated.likelihood(b) - 1.0 / 3.0) < 1e-9)

    assert(updated.possibilities.length == 2)
    assert(math.abs(updated.possibilities.map(_.likelihood).sum - 1.0) < 1e-9)
  }

  "An EliminationHand" should "preserve probabilities for neutral tile weights" in {
    val a = Domino(5, 1)
    val b = Domino(4, 1)

    val hand = EliminationHand.createFromOpponentHand(
      "human",
      Domino.all().filterNot(Set(a, b).contains),
      1,
    )

    val updated = hand.applyTileWeights(
      Map(a -> 1.0, b -> 1.0),
    )

    assert(math.abs(updated.likelihood(a) - 0.5) < 1e-9)
    assert(math.abs(updated.likelihood(b) - 0.5) < 1e-9)
  }

  "Bayesian inference" should "assign higher weights to tiles associated with good moves" in {
    val overallGoodRate = 0.5

    val favorable = EliminationHand.estimateTileWeight(
      goodWithTile = 9,
      totalWithTile = 10,
      overallGoodRate = overallGoodRate,
    )

    val unfavorable = EliminationHand.estimateTileWeight(
      goodWithTile = 1,
      totalWithTile = 10,
      overallGoodRate = overallGoodRate,
    )

    val neutral = EliminationHand.estimateTileWeight(
      goodWithTile = 5,
      totalWithTile = 10,
      overallGoodRate = overallGoodRate,
    )

    assert(math.abs(favorable - 1.4) < 1e-9)
    assert(math.abs(unfavorable - 0.6) < 1e-9)
    assert(math.abs(neutral - 1.0) < 1e-9)
  }

  "An EliminationHand" should "combine duplicate hands when normalizing" in {
    val a = Domino(5, 1)
    val b = Domino(4, 1)

    val hand = EliminationHand(
      "human",
      Vector(
        PossibleHand(Set(a), 0.2, 0.0),
        PossibleHand(Set(a), 0.3, 0.0),
        PossibleHand(Set(b), 0.5, 0.0),
      ),
      Set.empty,
    )

    val normalized = hand.normalize()

    assert(normalized.possibilities.length == 2)

    val probabilities = normalized.possibilities.map(ph => ph.hand -> ph.likelihood).toMap

    assert(math.abs(probabilities(Set(a)) - 0.5) < 1e-9)
    assert(math.abs(probabilities(Set(b)) - 0.5) < 1e-9)
    assert(math.abs(normalized.possibilities.map(_.likelihood).sum - 1.0) < 1e-9)

    val cumulative = normalized.possibilities.map(_.cumulativeLikelihood)

    assert(math.abs(cumulative.head - 0.5) < 1e-9)
    assert(math.abs(cumulative.last - 1.0) < 1e-9)
  }

  "An EliminationHand" should "sample according to nonuniform probabilities" in {
    val a = Domino(5, 1)
    val b = Domino(4, 1)
    val c = Domino(3, 1)

    val hand = EliminationHand(
      "human",
      Vector(
        PossibleHand(Set(a), 0.1, 0.1),
        PossibleHand(Set(b), 0.3, 0.4),
        PossibleHand(Set(c), 0.6, 1.0),
      ),
      Set.empty,
    )

    val n = 10000

    val counts = (1 to n)
      .map(_ => hand.getSample().head)
      .groupMapReduce(identity)(_ => 1)(_ + _)

    def frequency(tile: Domino): Double =
      counts.getOrElse(tile, 0).toDouble / n

    assert(math.abs(frequency(a) - 0.1) < 0.02)
    assert(math.abs(frequency(b) - 0.3) < 0.02)
    assert(math.abs(frequency(c) - 0.6) < 0.02)
  }

  "An EliminationHand" should "return an empty sample when no hands are possible" in {
    val hand = EliminationHand(
      "human",
      Vector.empty,
      Set.empty,
    )

    assert(hand.getSample() == Nil)
  }

  "An EliminationHand" should "always sample the only possible hand" in {
    val tiles = Set(Domino(5, 1), Domino(4, 1))

    val hand = EliminationHand(
      "human",
      Vector(PossibleHand(tiles, 1.0, 1.0)),
      Set.empty,
    )

    val samples = (1 to 100).map(_ => hand.getSample().toSet)

    assert(samples.forall(_ == tiles))
  }

  "An EliminationHand" should "normalize unique hands and calculate cumulative probabilities" in {
    val a = Domino(5, 1)
    val b = Domino(4, 1)
    val c = Domino(3, 1)

    val hand = EliminationHand(
      "human",
      Vector(
        PossibleHand(Set(a), 2.0, 0.0),
        PossibleHand(Set(b), 3.0, 0.0),
        PossibleHand(Set(c), 5.0, 0.0),
      ),
      Set.empty,
    )

    val normalized = hand.normalizeUnique()

    val probabilities = normalized.possibilities.map(_.likelihood)
    val cumulative = normalized.possibilities.map(_.cumulativeLikelihood)

    assert(probabilities.zip(Vector(0.2, 0.3, 0.5)).forall { case (actual, expected) =>
      math.abs(actual - expected) < 1e-9
    })

    assert(cumulative.zip(Vector(0.2, 0.5, 1.0)).forall { case (actual, expected) =>
      math.abs(actual - expected) < 1e-9
    })

    assert(normalized.possibilities.map(_.hand) == hand.possibilities.map(_.hand))
  }
}
