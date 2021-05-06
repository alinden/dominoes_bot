package game

import scala.util.Random
import cats.implicits._

case class EliminationHand(
  playerName: String,
  possibilities: List[PossibleHand],
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
      List()
    } else {
      val xs = this.possibilities
      val rv = Random.nextDouble()
      var low = 0
      var high = xs.length - 1
      var index = -1
      while ((index == -1) && (low <= high)) {
        val middle = low + (high - low) / 2
        val x = xs(middle)
        val lowerBound = x.cumulativeLikelihood - x.likelihood
        val upperBound = x.cumulativeLikelihood
        if (lowerBound <= rv && rv <= upperBound) {
          index = middle
        } else if (xs(middle).cumulativeLikelihood < rv) {
          // Search elements with higher cumulative likelihoods
          low = middle + 1
        } else {
          // Search elements with lower cumulative likelihoods
          high = middle - 1
        }
      }
      if (index == -1) {
        throw new Error("index == -1 in binary search")
      }
      xs(index).hand.toList
    }
  }
  def normalize(): EliminationHand = {
    if (possibilities.isEmpty) {
      this
    } else {
      val totalLikelihood = possibilities.map(_.likelihood).sum
      val multiplier = 1.0/totalLikelihood
      var cl = 0.0
      val m: Map[Set[Domino], Double] = possibilities
        .map((ph) => Map(ph.hand -> ph.likelihood))
        .reduce(_ |+| _)
      EliminationHand(
        playerName,
        m.toList.map{ case (hand, likelihood) => {
          cl = cl + multiplier*likelihood
          PossibleHand(hand, multiplier*likelihood, cl)
        }}.sortBy(_.cumulativeLikelihood),
        impossible,
      )
    }
  }
  def updateForOpponentMove(tile: Domino): EliminationHand = {
    EliminationHand(
      playerName,
      possibilities.filterNot((ph) => ph.contains(tile)),
      impossible + tile,
    )
  }
  def updateForDraw(entry: DrawLogEntry, debug: Boolean = false): EliminationHand = {
    if (entry.playerName != playerName) {
      this
    } else {
      val updatedPossibilities = possibilities.filter((ph) =>
          entry.legalTilesToPlay.toSet.intersect(ph.hand).isEmpty
        ).flatMap((ph) => {
          val remainingTiles = Domino.all()
            .filterNot((d) => impossible.contains(d) || ph.hand.contains(d))
          remainingTiles.map((d) => ph.addTile(d))
        })
      EliminationHand(
        playerName,
        updatedPossibilities,
        impossible,
      )
    }
  }
  def updateForHighestDouble(entry: HighestDoubleEntry): EliminationHand = {
    val tile = entry.highestDouble
    val higherDoubles = Domino.doubles()
      .filter((d) => d.high > tile.high)
    if (playerName == entry.playerName) {
      EliminationHand(
        playerName,
        possibilities.flatMap((ph) => {
          if (ph.contains(tile) && !higherDoubles.exists((hd) => ph.contains(hd))) {
            List(ph.removeTile(tile))
          } else {
            List()
          }
        }),
        impossible + tile,
      )
    } else {
      val tileAndHigherDoubles = tile::higherDoubles
      EliminationHand(
        playerName,
        possibilities.filterNot((ph) => (tileAndHigherDoubles).exists((hd) => ph.contains(hd))),
        impossible + tile,
      )
    }
  }
  def updateForMove(entry: MoveLogEntry, before: Game, skipBayes: Boolean = true): EliminationHand = {
    if (entry.playerName == playerName) {
      updateForSelfMove(entry.move, before, skipBayes)
    } else {
      updateForOpponentMove(entry.move.domino)
    }
  }
  def updateForSelfMove(move: Move, before: Game, skipBayes: Boolean = true): EliminationHand = {
    val default = EliminationHand(
        playerName,
        possibilities.flatMap((ph) => {
          if (ph.contains(move.domino)) {
            List(ph.removeTile(move.domino))
          } else {
            List()
          }
        }),
        impossible + move.domino,
      ).normalize()
    if (skipBayes) {
      default
    } else {
      val numSamples = 100
      val samples = (0 to numSamples).toList.map((_) => {
        move.domino::default.getSample()
      })
      val handsAndGoodActions: List[(List[Domino], List[Action])] = samples.map((sample) => {
        (sample, MinNode(
          Perspective(
            "human",
            "robot",
            sample,
            None,
            EliminationHand.createFromOpponentHand("robot", sample ++ before.board.tiles()),
            Some(this),
          ),
          before,
          1,
          32,
          true,
        ).goodActions())
      })
      val handsWhereMoveIsBad: List[List[Domino]] = handsAndGoodActions.flatMap{ case (hand, goodActions) => {
        if (goodActions.contains(move)) {
          List()
        } else {
          List(hand)
        }
      }}
      val handsWhereMoveIsGood: List[List[Domino]] = handsAndGoodActions.flatMap{ case (hand, goodActions) => {
        if (goodActions.contains(move)) {
          List(hand)
        } else {
          List()
        }
      }}
      val unlikelyTiles: Set[Domino] = handsWhereMoveIsBad.flatten.toSet.diff(handsWhereMoveIsGood.flatten.toSet)
      val likelyTiles: Set[Domino] = handsWhereMoveIsGood.flatten.toSet.diff(handsWhereMoveIsBad.flatten.toSet)
      if (unlikelyTiles.isEmpty) {
        default
      } else {
        EliminationHand(
          playerName,
          (default.possibilities.filter((ph) => ph.hand.intersect(unlikelyTiles union likelyTiles).isEmpty) ++
            default.possibilities.filterNot((ph) => ph.hand.intersect(unlikelyTiles).isEmpty).map((ph) => ph.multiplyLikelihood(0.2)) ++
            default.possibilities.filterNot((ph) => ph.hand.intersect(likelyTiles).isEmpty).map((ph) => ph.multiplyLikelihood(2.0))
            ),
          default.impossible,
        ).normalize()
      }
    }
  }
  def quickUpdateForEntry(entry: GameLogEntry, before: Game): EliminationHand = {
    updateForEntry(entry, before, true)
  }
  def updateForEntry(entry: GameLogEntry, before: Game, skipBayes: Boolean = false, debug: Boolean = false): EliminationHand = entry match {
    case x: MoveLogEntry => updateForMove(x, before, skipBayes).normalize()
    case x: HighestDoubleEntry => updateForHighestDouble(x).normalize()
    case x: DrawLogEntry => updateForDraw(x, debug).normalize()
    case _ => {
      this
    }
  }
  def eliminateTile(tile: Domino): EliminationHand = {
    EliminationHand(
      playerName,
      possibilities.filterNot((ph) => ph.contains(tile)),
      impossible + tile,
    ).normalize()
  }
}

object EliminationHand {
  def createFromOpponentHand(
    playerName: String,
    opponentHand: List[Domino],
    initialHandSize: Int = 7
  ): EliminationHand = {
    val impossible = opponentHand.toSet
    val possibleTiles = Domino.all().filterNot(impossible.contains(_)).toSet
    val possibleHands = possibleTiles.subsets(initialHandSize).toList
    val likelihood = 1.0/possibleHands.length.toFloat
    var totalLikelihood = 0.0
    EliminationHand(
      playerName,
      possibleHands.map((hand) => {
        totalLikelihood += likelihood
        PossibleHand(hand, likelihood, totalLikelihood)
      }),
      impossible,
    ).normalize()
  }
}
