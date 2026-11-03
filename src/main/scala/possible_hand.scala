package game

case class PossibleHand(
    mask: Int,
    likelihood: Double,
    cumulativeLikelihood: Double
) {

  def hand: Set[Domino] =
    Domino.all().iterator
      .filter(d => HandMask.contains(mask, d))
      .toSet

  def multiplyLikelihood(x: Double): PossibleHand =
    copy(likelihood = x * likelihood)

  def addTile(tile: Domino): PossibleHand =
    copy(mask = HandMask.add(mask, tile))

  def removeTile(tile: Domino): PossibleHand =
    copy(mask = HandMask.remove(mask, tile))

  def setCumulativeLikelihood(x: Double): PossibleHand =
    copy(cumulativeLikelihood = x)

  def contains(tile: Domino): Boolean =
    HandMask.contains(mask, tile)

  override def toString(): String =
    s"${likelihood * 100}%: ${hand.mkString(",")}"
}

object PossibleHand {
  def apply(
      hand: Set[Domino],
      likelihood: Double,
      cumulativeLikelihood: Double
  ): PossibleHand =
    new PossibleHand(
      HandMask.fromHand(hand),
      likelihood,
      cumulativeLikelihood
    )
}
