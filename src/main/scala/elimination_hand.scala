package game

import scala.util.Random
import cats.implicits._
import scala.collection.mutable

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
      hands: Vector[(PossibleHand, Double)],
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
      possibilities.map(ph => (ph, ph.likelihood)),
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

  private def logObservations(
    observations: List[BayesObservation]
  ): Unit = {

    def sortedTiles(hand: Set[Domino]): List[Domino] =
      hand.toList.sortBy(d => (d.high, d.low))

    def logGroup(
      label: String,
      group: List[BayesObservation]
    ): Unit = {
      println(s"[Bayes] $label (${group.size})")

      group
        .sortBy(o => sortedTiles(o.hand).map(d => (d.high, d.low)))
        .foreach { observation =>
          val hand = sortedTiles(observation.hand).mkString(" ")

          val scores = observation.scoredMoves
            .sortWith { case ((a, _), (b, _)) =>
              a.value < b.value // MinNode: lower is better
            }
            .map { case (score, move) =>
              val preferred =
                if (observation.preferredMoves.contains(move)) "*" else ""

              f"$move: ${score.value}%.3f$preferred"
            }
            .mkString(", ")

          if (observation.evaluation == ForcedMove) {
            println(s"  $hand | FORCED")
          } else {
            println(s"  $hand | $scores")
          }
        }
    }

    logGroup("GOOD", observations.filter(_.evaluation == GoodMove))
    logGroup("BAD", observations.filter(_.evaluation == BadMove))
    logGroup("FORCED", observations.filter(_.evaluation == ForcedMove))
  }

  private def updateUsingTileEvidence(
    updated: EliminationHand,
    move: Move,
    before: Game,
    config: BayesConfig
  ): EliminationHand = {

    val observations: List[BayesObservation] =
      (0 until config.sample).toList.map { _ =>
        val remainingHand = updated.getSample()
        evaluateMove(move, before, remainingHand.toSet, config)
      }

    // Log all observations, including forced moves.
    logObservations(observations)

    // Forced moves provide no evidence about strategic preferences.
    val informative = observations.filterNot(_.evaluation == ForcedMove)

    val goodCount = informative.count(_.evaluation == GoodMove)
    val badCount = informative.count(_.evaluation == BadMove)
    val forcedCount = observations.size - informative.size

    println(
      s"[Bayes] GOOD=$goodCount BAD=$badCount FORCED=$forcedCount"
    )

    // No useful evidence if every informative observation agrees,
    // or if all observations were forced.
    if (goodCount == 0 || badCount == 0) {
      return updated
    }

    val overallGoodRate = goodCount.toDouble / informative.size

    val tileCounts = mutable.Map.empty[Domino, (Int, Int)]

    informative.foreach { observation =>
      val isGood = observation.evaluation == GoodMove

      observation.hand.foreach { tile =>
        val (total, good) = tileCounts.getOrElse(tile, (0, 0))

        tileCounts(tile) = (
          total + 1,
          good + (if (isGood) 1 else 0)
        )
      }
    }

    val tileWeights = tileCounts.iterator.map {
      case (tile, (total, good)) =>
        tile -> EliminationHand.estimateTileWeight(
          goodWithTile = good,
          totalWithTile = total,
          overallGoodRate = overallGoodRate,
          priorStrength = Config.bayesPriorStrength
        )
    }.toMap

    updated.applyTileWeights(
      tileWeights,
      Config.bayesEvidenceStrength
    )
  }

  def applyHandEvidence(
    evaluations: Vector[(PossibleHand, Option[Boolean])],
    badMoveWeight: Double = Config.bayesBadMoveWeight,
  ): EliminationHand = {
    require(badMoveWeight > 0.0 && badMoveWeight <= 1.0)

    val badCount = evaluations.count(_._2.contains(false))

    // No update if all informative evaluations agree.
    if (badCount == 0 || badCount == evaluations.size) {
      this
    } else {
      val adjusted = evaluations.map { case (ph, evaluation) =>
        val weight = evaluation match {
          case Some(true)  => 1.0
          case Some(false) => badMoveWeight
          case None        => 1.0
        }

        ph.multiplyLikelihood(weight)
      }

      copy(possibilities = adjusted).normalizeUnique()
    }
  }

  private def updateHandsDirectly(
    updated: EliminationHand,
    move: Move,
    before: Game,
    config: BayesConfig
  ): EliminationHand = {

    val evaluations = updated.possibilities.map { ph =>
      val observation = evaluateMove(move, before, ph.hand, config)
      (ph, observation)
    }

    logObservations(evaluations.map(_._2).toList)

    val evidence = evaluations.map { case (ph, observation) =>
      val result = observation.evaluation match {
        case GoodMove   => Some(true)
        case BadMove    => Some(false)
        case ForcedMove => None
      }

      ph -> result
    }

    updated.applyHandEvidence(evidence)
  }

  private def evaluateMove(
    move: Move,
    before: Game,
    remainingHand: Set[Domino],
    config: BayesConfig
  ): BayesObservation = {

    val originalHand = move.domino :: remainingHand.toList
    val hypotheticalGame = before.withReplacedActiveHand(originalHand)
    val legalMoves = hypotheticalGame.legalMoves()
    val simplifiedMoves = hypotheticalGame.simplify(legalMoves).collect{ case m: Move => m}

    if (simplifiedMoves.size == 1) {
      BayesObservation(
        remainingHand,
        ForcedMove,
        List(move),
        List.empty
      )
    } else {

      val knownTiles = originalHand ++ before.board.tiles()

      val unknownCount = 28 - knownTiles.distinct.size

      val robotHandSize = before.playerByName("robot").hand.size

      val possibleHands =
        EliminationHand.combinationCount(unknownCount, robotHandSize)

      val opponentHand =
        if (possibleHands <= Config.bayesOpponentHandSamples) {
          EliminationHand.createFromOpponentHand("robot", knownTiles)
        } else {
          EliminationHand.sampleFromOpponentHand(
            "robot",
            knownTiles,
            numSamples = Config.bayesOpponentHandSamples
          )
        }

      val perspective = Perspective(
        "human",
        "robot",
        originalHand,
        None,
        opponentHand,
        Some(this)
      )

      val node = MinNode(
        perspective,
        hypotheticalGame,
        config.depth,
        config.simulations,
        true
      )

      val scores = node.scoredMoves(simplifiedMoves)
      val preferredMoves = node.goodMoves(scores)

      BayesObservation(
        remainingHand,
        if (preferredMoves.contains(move)) GoodMove else BadMove,
        preferredMoves,
        scores
      )
    }
  }

  private def bayesConfig(handCount: Int): BayesConfig =
    Config.bayesConfigs.reverse.find { config =>
      config.handThreshold.forall(handCount <= _)
    }.get

  def updateForSelfMove(
    move: Move,
    before: Game,
    skipBayes: Boolean = true,
  ): EliminationHand = {

    val updated = EliminationHand(
      playerName,
      possibilities.collect {
        case ph if ph.contains(move.domino) =>
          ph.removeTile(move.domino)
      },
      impossible + move.domino,
    ).normalizeUnique()

    val handCount = updated.possibilities.size

    if (skipBayes || handCount == 0) {
      updated
    } else {
      val config = bayesConfig(updated.possibilities.size)

      EliminationHand.benchmark(
        s"${config.phaseName} | " +
        s"method=${config.method} | " +
        s"hands=${updated.possibilities.size} | " +
        s"depth=${config.depth} | " +
        s"simulations=${config.simulations}"
      ) {
        config.method match {
          case DirectBayes =>
            updateHandsDirectly(updated, move, before, config)

          case TileBayes =>
            updateUsingTileEvidence(updated, move, before, config)
        }
      }
    }
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

  private def benchmark[A](label: String)(f: => A): A = {
    if (!Config.benchmarkBayes) {
      f
    } else {
      val start = System.nanoTime()

      try {
        f
      } finally {
        val elapsedMs = (System.nanoTime() - start) / 1e6
        println(f"[Bayes] $label: $elapsedMs%.1f ms")
      }
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

}
