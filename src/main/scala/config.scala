package game

case class BayesConfig(
  phaseName: String,
  method: BayesMethod,
  depth: Int,
  simulations: Int,
  sample: Int,
  handThreshold: Option[Int],
)

sealed trait BayesMethod
case object DirectBayes extends BayesMethod
case object TileBayes extends BayesMethod

object Config {

  // Larger values produce more conservative estimates.
  val bayesPriorStrength = 10.0
  // Reduce the strength of the combined tile evidence.
  val bayesEvidenceStrength = 1.0
  // Bayesian inference
  val bayesConfigs: List[BayesConfig] = List(
    BayesConfig("opening", TileBayes, 3, 8, 50, None),
    BayesConfig("early middlegame", TileBayes, 5, 10, 50, Some(1000)),
    BayesConfig("late middlegame", DirectBayes, 7, 10, 50, Some(200)),
    BayesConfig("endgame", DirectBayes, 9, 10, 50, Some(50)),
  )

  // If there are more possible hands than this, then we use
  // sampling to construct the initial distribution before
  // bayesian anlayis.
  val bayesOpponentHandSamples = 200
  // Relative likelihood assigned to hands where the observed move
  // is classified as bad. Nonzero because search evaluations are approximate.
  val bayesBadMoveWeight = 0.2

  // Log Bayesian inference performance.
  val benchmarkBayes = false
  // Log Bayesian inference at a hand by hand level.
  val logBayes = false

  // Robot move selection
  // List of (depth, breadth) tuples
  val moveParams = List((5, 10), (7, 20), (9, 20), (11, 20), (15, 30))
  // Continue to the next (depth, breadth) pair if computation
  // takes less than this amount of time.
  val moveTimeMs = 3000.0

  def nextBreadth(breadth: Int): Int = math.max(1, (breadth + 1) / 2)

  // strategic bonus to give to going first in the next round
  val dominoesBonus = 10.0
}
