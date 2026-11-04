package game

case class Domino(high: Int, low: Int) {

  override def toString(): String = {
    s"[${this.high},${this.low}]"
  }

  def isDouble(): Boolean = {
    this.high == this.low
  }

}

object Domino {
  private val allDominoes: List[Domino] =
    (for {
      high <- 0 to 6
      low <- 0 to high
    } yield Domino(high, low)).toList

  private val doubleDominoes = allDominoes.filter(_.isDouble())

  def all(): List[Domino] = allDominoes

  def doubles(): List[Domino] = doubleDominoes
}

