package game

trait GameLogEntry {
  val message: String = s"${playerName}: ${action}"
  val playerName: String
  val action: Action
}

case class DrawLogEntry(
    playerName: String,
    playerHandSize: Int,
    legalTilesToPlay: List[Domino]
) extends GameLogEntry {
  val action: Action = Draw
  override def toString(): String =
    s"${message}: ${legalTilesToPlay.mkString(" ")}"
}

case class PassLogEntry(playerName: String) extends GameLogEntry {
  val action: Action = Pass
}

case class MoveLogEntry(playerName: String, move: Move) extends GameLogEntry {
  val action: Action = move
}

case class HighestDoubleEntry(
    playerName: String,
    highestDouble: Domino
) extends GameLogEntry {
  val action: Action = PlayHighestDouble
  override def toString(): String =
    s"${playerName}: Play Highest Double ${highestDouble}"
}

case class GameLog(log: List[GameLogEntry]) {
  def log(entry: GameLogEntry): GameLog = GameLog(entry :: this.log)
}

object GameLog {
  def empty(): GameLog = GameLog(List())
}
