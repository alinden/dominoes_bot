package game

object Config {

  // Bayesian inference
  val bayesDepth = 3
  val bayesSamples = 50
  val bayesSimulations = 20
  val bayesInitialHandSamples = 200
  // Larger values produce more conservative estimates.
  val bayesPriorStrength = 10.0
  // Reduce the strength of the combined tile evidence.
  val bayesEvidenceStrength = 0.5

  // Robot move selection
  // List of (depth, breadth) tuples
  val moveParams = List((5, 10), (7, 20), (9, 20), (11, 20), (15, 30))
  // Continue to the next (depth, breadth) pair if computation
  // takes less than this amount of time.
  val moveTimeMs = 3000.0

  def nextBreadth(breadth: Int): Int = math.max(1, (breadth + 1) / 2)
}
