package game

import scala.util.Random
import cats.implicits._

case class EliminationHand(
    playerName: String,
    possibilities: Vector[PossibleHand],
    impossible: Set[Domino],
) {
  override def toString(): String = {
    val normalized = this.normalize()
    s"${normalized.possibilities.length} possible hands, including:\n${normalized.possibilities.sortBy((ph) => -ph.likelihood).take(10).mkString("\n")}"
  }
  def length(): Int = possibilities.head.hand.size
  def contains(tile: Domino): Boolean = {
    possibilities.exists((ph) => ph.contains(tile))
  }
  def likelihood(tile: Domino): Double = {
    possibilities
      .map((ph) => if (ph.contains(tile)) ph.likelihood else 0.0)
      .sum
  }
  def truncate(): EliminationHand = {
    if (possibilities.length < 1000) {
      this
    } else {
      EliminationHand(
        playerName,
        Random.shuffle(possibilities).take(1000),
        impossible,
      )
    }
  }

  def getSample(): List[Domino] = {
    if (possibilities.isEmpty) {
      Nil
    } else {
      val rv = Random.nextDouble()

      @annotation.tailrec
      def search(low: Int, high: Int): Int = {
        if (low > high) {
          throw new IllegalStateException("No hand found for sampled probability")
        }

        val middle = low + (high - low) / 2
        val ph = possibilities(middle)

        if (
          rv >= ph.cumulativeLikelihood - ph.likelihood &&
          rv <= ph.cumulativeLikelihood
        ) {
          middle
        } else if (rv > ph.cumulativeLikelihood) {
          search(middle + 1, high)
        } else {
          search(low, middle - 1)
        }
      }

      val index = search(0, possibilities.length - 1)
      possibilities(index).hand.toList
    }
  }

  private def normalizeProbabilities(
    hands: Vector[(PossibleHand, Double)]
  ): EliminationHand = {
    if (hands.isEmpty) {
      this
    } else {
      val total = hands.iterator.map(_._2).sum

      val probabilities = hands.map { case (ph, likelihood) =>
        (ph, likelihood / total)
      }

      val cumulative = probabilities
        .map(_._2)
        .scanLeft(0.0)(_ + _)
        .tail

      val normalized = probabilities
        .zip(cumulative)
        .map { case ((ph, probability), cumulativeLikelihood) =>
          ph.copy(
            likelihood = probability,
            cumulativeLikelihood = cumulativeLikelihood,
          )
        }

      copy(possibilities = normalized)
    }
  }

  def normalize(): EliminationHand = {
    val combined = possibilities
      .groupMapReduce(_.mask)(ph => (ph, ph.likelihood)) {
        case ((ph, likelihood1), (_, likelihood2)) =>
          (ph, likelihood1 + likelihood2)
      }

    normalizeProbabilities(combined.valuesIterator.toVector)
  }

  def normalizeUnique(): EliminationHand =
    normalizeProbabilities(
      possibilities.map(ph => (ph, ph.likelihood))
    )

  def updateForOpponentMove(tile: Domino): EliminationHand = {
    EliminationHand(
      playerName,
      possibilities.filterNot((ph) => ph.contains(tile)),
      impossible + tile,
    )
  }
  def updateForDraw(
      entry: DrawLogEntry,
      debug: Boolean = false,
  ): EliminationHand = {
    if (entry.playerName != playerName) {
      this
    } else {
      val legalMask = HandMask.fromHand(entry.legalTilesToPlay)
      val eligibleTiles =
        Domino
          .all()
          .filterNot(impossible.contains)
          .map(d => (d, HandMask.tileBit(d)))

      val updatedPossibilities = possibilities
        .filter(ph => !HandMask.intersects(ph.mask, legalMask))
        .flatMap { ph =>
          eligibleTiles.collect {
            case (d, bit) if (ph.mask & bit) == 0 =>
              ph.addTile(d)
          }
        }

      EliminationHand(
        playerName,
        updatedPossibilities,
        impossible,
      )
    }
  }
  def updateForHighestDouble(entry: HighestDoubleEntry): EliminationHand = {
    val tile = entry.highestDouble
    val higherDoubles = Domino
      .doubles()
      .filter((d) => d.high > tile.high)
    if (playerName == entry.playerName) {
      EliminationHand(
        playerName,
        possibilities.flatMap((ph) => {
          if (ph.contains(tile) && !higherDoubles.exists((hd) => ph.contains(hd))) {
            Vector(ph.removeTile(tile))
          } else {
            Vector()
          }
        }),
        impossible + tile,
      )
    } else {
      val tileAndHigherDoubles = tile :: higherDoubles
      EliminationHand(
        playerName,
        possibilities.filterNot((ph) => (tileAndHigherDoubles).exists((hd) => ph.contains(hd))),
        impossible + tile,
      )
    }
  }
  def updateForMove(
      entry: MoveLogEntry,
      before: Game,
      skipBayes: Boolean = true,
  ): EliminationHand = {
    if (entry.playerName == playerName) {
      updateForSelfMove(entry.move, before, skipBayes)
    } else {
      updateForOpponentMove(entry.move.domino)
    }
  }

  def updateForSelfMove(
      move: Move,
      before: Game,
      skipBayes: Boolean = true,
  ): EliminationHand = {

    // Eliminate hands that couldn't have played this tile,
    // then remove the played tile from the remaining hands.
    val updated = EliminationHand(
      playerName,
      possibilities.collect {
        case ph if ph.contains(move.domino) =>
          ph.removeTile(move.domino)
      },
      impossible + move.domino,
    ).normalizeUnique()

    if (skipBayes || updated.possibilities.isEmpty) {
      return updated
    }

    // ------------------------------------------------------------
    // Sample hypothetical hands and evaluate the observed move.
    // ------------------------------------------------------------

    val observations: List[(Set[Domino], Boolean)] =
      (0 until Config.bayesSamples).toList.map { _ =>
        val remainingHand = updated.getSample()
        val originalHand = move.domino :: remainingHand

        val goodActions = MinNode(
          Perspective(
            "human",
            "robot",
            originalHand,
            None,
            EliminationHand.sampleFromOpponentHand(
              "robot",
              originalHand ++ before.board.tiles(),
              numSamples = Config.bayesInitialHandSamples,
            ),
            Some(this),
          ),
          before,
          Config.bayesDepth,
          Config.bayesSimulations,
          true, // isRoot
        ).goodActions()

        (
          remainingHand.toSet,
          goodActions.contains(move),
        )
      }

    // ------------------------------------------------------------
    // Estimate P(good move | tile) for each observed tile.
    // ------------------------------------------------------------

    val goodCount = observations.count(_._2)

    // If every sampled hand produces the same result,
    // there is no useful distinction to learn.
    if (goodCount == 0 || goodCount == Config.bayesSamples) {
      return updated
    }

    val overallGoodRate = goodCount.toDouble / Config.bayesSamples

    val priorStrength = Config.bayesPriorStrength

    val evidenceStrength = Config.bayesEvidenceStrength

    val tileCounts = scala.collection.mutable.Map.empty[
      Domino,
      (Int, Int),
    ]

    observations.foreach { case (hand, isGood) =>
      hand.foreach { tile =>
        val (total, good) = tileCounts.getOrElse(tile, (0, 0))

        tileCounts(tile) = (
          total + 1,
          good + (if (isGood) 1 else 0),
        )
      }
    }

    val tileWeights: Map[Domino, Double] =
      tileCounts.iterator.map { case (tile, (total, good)) =>
        tile -> EliminationHand.estimateTileWeight(
          goodWithTile = good,
          totalWithTile = total,
          overallGoodRate = overallGoodRate,
          priorStrength = priorStrength,
        )
      }.toMap

    // ------------------------------------------------------------
    // Apply the inferred tile weights to every possible hand.
    // ------------------------------------------------------------

    updated.applyTileWeights(tileWeights, evidenceStrength)
  }

  def quickUpdateForEntry(
      entry: GameLogEntry,
      before: Game,
  ): EliminationHand = {
    updateForEntry(entry, before, true)
  }
  def updateForEntry(
      entry: GameLogEntry,
      before: Game,
      skipBayes: Boolean = false,
      debug: Boolean = false,
  ): EliminationHand = entry match {
    case x: MoveLogEntry => {
      val updated = updateForMove(x, before, skipBayes)
      if (skipBayes) updated.normalizeUnique()
      else updated.normalize()
    }
    case x: HighestDoubleEntry => updateForHighestDouble(x).normalizeUnique()
    case x: DrawLogEntry       => updateForDraw(x, debug).normalize()
    case _ => {
      this
    }
  }
  def eliminateTile(tile: Domino): EliminationHand = {
    EliminationHand(
      playerName,
      possibilities.filterNot((ph) => ph.contains(tile)),
      impossible + tile,
    ).normalizeUnique()
  }

  // Testable method to adjust hand likelihoods
  def applyTileWeights(
      tileWeights: Map[Domino, Double],
      evidenceStrength: Double = 0.5,
  ): EliminationHand = {
    val adjusted = possibilities.map { ph =>
      val logWeight = ph.hand.iterator.map { tile =>
        math.log(tileWeights.getOrElse(tile, 1.0))
      }.sum

      val multiplier = math.exp(logWeight * evidenceStrength)

      ph.multiplyLikelihood(multiplier)
    }

    copy(possibilities = adjusted).normalizeUnique()
  }

  def decisionLogSummary(actualHand: Set[Domino]): String = {
    val sorted = possibilities.sortBy(-_.likelihood)
    val total = sorted.size

    if (total == 0) {
      return "No possible hands"
    }

    val uniformProbability = 1.0 / total

    // Marginal probability of each tile
    val tileProbabilities =
      sorted
        .flatMap(ph => ph.hand.toList.map(_ -> ph.likelihood))
        .groupMapReduce(_._1)(_._2)(_ + _)
        .toList
        .sortBy(-_._2)

    val mostLikelyTiles = tileProbabilities.take(5)
    val leastLikelyTiles = tileProbabilities.takeRight(5).reverse

    val actualProbability = sorted
      .find(_.hand == actualHand)
      .map(_.likelihood)
      .getOrElse(0.0)

    val actualRank = sorted.indexWhere(_.hand == actualHand) + 1

    def formatProbability(p: Double): String =
      f"${p * 100}%.3f%%"

    val lines = List(
      s"$total possible hands",
      s"Uniform hand probability: ${formatProbability(uniformProbability)}",
      "",
      "Most likely tiles:",
    ) ++
      mostLikelyTiles.map { case (tile, p) =>
        s"  $tile  ${formatProbability(p)}"
      } ++
      List("", "Least likely tiles:") ++
      leastLikelyTiles.map { case (tile, p) =>
        s"  $tile  ${formatProbability(p)}"
      } ++
      List("", "Most likely hands:") ++
      sorted.take(5).map { ph =>
        s"  ${formatProbability(ph.likelihood)}  ${ph.hand.mkString(" ")}"
      } ++
      List(
        "",
        "Actual hand:",
        s"  ${actualHand.mkString(" ")}",
        s"  Probability: ${formatProbability(actualProbability)}",
        f"  Relative to uniform: ${actualProbability / uniformProbability}%.2fx",
        s"  Rank: $actualRank / $total",
      )

    lines.mkString("\n")
  }
}

object EliminationHand {
  def createFromOpponentHand(
    playerName: String,
    opponentHand: List[Domino],
    initialHandSize: Int = 7,
  ): EliminationHand = {
    val impossible = opponentHand.toSet
    val possibleTiles = Domino.all().filterNot(impossible.contains).toSet
    val possibleHands = possibleTiles.subsets(initialHandSize).toVector

    if (possibleHands.isEmpty) {
      EliminationHand(playerName, Vector.empty, impossible)
    } else {
      // All hands consistent with the known
      // excluded tiles are equally likely as this method is called to create
      // the initial probability distribution - i.e. we currently have no
      // information on which hands are likelier than others.
      val likelihood = 1.0 / possibleHands.length.toDouble

      val possibilities = possibleHands.zipWithIndex.map { case (hand, index) =>
        val cumulativeLikelihood =
          if (index == possibleHands.length - 1) 1.0
          else (index + 1) * likelihood

        PossibleHand(hand, likelihood, cumulativeLikelihood)
      }

      EliminationHand(playerName, possibilities, impossible)
    }
  }

  def sampleFromOpponentHand(
      playerName: String,
      opponentHand: List[Domino],
      initialHandSize: Int = 7,
      numSamples: Int = 1000,
  ): EliminationHand = {
    val impossible = opponentHand.toSet
    val possibleTiles = Domino.all().filterNot(impossible.contains).toVector

    if (possibleTiles.length < initialHandSize || numSamples <= 0) {
      EliminationHand(playerName, Vector.empty, impossible)
    } else {
      val random = scala.util.Random

      val sampledHands = scala.collection.mutable.HashSet.empty[Set[Domino]]

      val maxHands = combinationCount(possibleTiles.length, initialHandSize)
      val target = math.min(numSamples.toLong, maxHands).toInt

      while (sampledHands.size < target) {
        val hand = random
          .shuffle(possibleTiles)
          .take(initialHandSize)
          .toSet

        sampledHands += hand
      }

      // Similar to createFromOpponentHand, this method creates the initial distribution
      // from the initial assumption that all possible hands are equally likely.
      val likelihood = 1.0 / sampledHands.size

      val possibilities = sampledHands.iterator.zipWithIndex.map { case (hand, index) =>
        val cumulativeLikelihood =
          if (index == sampledHands.size - 1) 1.0
          else (index + 1) * likelihood

        PossibleHand(hand, likelihood, cumulativeLikelihood)
      }.toVector

      EliminationHand(playerName, possibilities, impossible)
    }
  }

  private def combinationCount(n: Int, k: Int): Long = {
    if (k < 0 || k > n) 0L
    else {
      val r = math.min(k, n - k)

      (1 to r).foldLeft(1L) { (result, i) =>
        result * (n - r + i) / i
      }
    }
  }

  def estimateTileWeight(
      goodWithTile: Int,
      totalWithTile: Int,
      overallGoodRate: Double,
      priorStrength: Double = 10.0,
  ): Double = {
    // Smoothed estimate of P(good move | tile).
    val pGoodGivenTile =
      (goodWithTile + priorStrength * overallGoodRate) /
        (totalWithTile + priorStrength)

    // Likelihood ratio relative to the overall good rate.
    pGoodGivenTile / overallGoodRate
  }
}
