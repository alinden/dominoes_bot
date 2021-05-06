package game

case class PossibleHand(hand: Set[Domino], likelihood: Double, cumulativeLikelihood: Double) {
  def multiplyLikelihood(x: Double): PossibleHand = {
    PossibleHand(hand, x*likelihood, cumulativeLikelihood)
  }
  def addTile(tile: Domino): PossibleHand = {
    PossibleHand(hand + tile, likelihood, cumulativeLikelihood)
  }
  def removeTile(tile: Domino): PossibleHand = {
    PossibleHand(hand - tile, likelihood, cumulativeLikelihood)
  }
  def setCumulativeLikelihood(x: Double) = {
    PossibleHand(hand, likelihood, x)
  }
  def contains(tile: Domino): Boolean = {
    hand.contains(tile)
  }
  override def toString(): String = {
    s"${likelihood*100}%: ${hand.mkString(",")}"
  }
}

