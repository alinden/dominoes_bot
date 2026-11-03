package game

sealed trait HeuristicMode

object HeuristicMode {
  case object LinearRunway extends HeuristicMode
  case object BoundedRunway extends HeuristicMode
}

object Heuristic {

  import HeuristicMode._

  val targetScore = 150.0
  val minimumRunway = 30.0
  val dominoesBonus = 5.0
  val sensitivity = 1.0

  def evaluate(
      robotScore: Double,
      humanScore: Double,
      gameOver: Boolean,
      dominoesWinner: Option[String] = None,
      mode: HeuristicMode = BoundedRunway
  ): Score = {

    if (gameOver) {
      Score(
        if (robotScore > humanScore) 1.0
        else if (robotScore < humanScore) -1.0
        else 0.0
      )
    } else {
      val bonus = dominoesWinner match {
        case Some("robot") => dominoesBonus
        case Some("human") => -dominoesBonus
        case _             => 0.0
      }

      val lead = robotScore - humanScore + bonus

      val runway =
        math.max(
          minimumRunway,
          targetScore - math.max(robotScore, humanScore)
        )

      val advantage = lead / runway

      val value = mode match {
        case LinearRunway =>
          advantage

        case BoundedRunway =>
          math.tanh(sensitivity * advantage)
      }

      Score(value)
    }
  }
}
