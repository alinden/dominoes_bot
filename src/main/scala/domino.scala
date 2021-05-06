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
  /**
   *  Return a list of all dominoes.
   */
  def all(): List[Domino] = {
    for {
      high <- (0 to 6).toList
      low <- (0 to 6).toList
      if (high >= low)
    } yield Domino(high, low)
  }

  /**
   *  Return a list of all doubles.
   */
  def doubles(): List[Domino] = {
    for {
      high <- (0 to 6).toList
    } yield Domino(high, high)
  }
}
