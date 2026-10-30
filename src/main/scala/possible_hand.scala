package game

case class PossibleHand(
    hand: Set[Domino],
    likelihood: Double,
    cumulativeLikelihood: Double,
    mask: Int,
) {
  def multiplyLikelihood(x: Double): PossibleHand = {
    copy(likelihood = x * likelihood)
  }

  def addTile(tile: Domino): PossibleHand = {
    copy(
      hand = hand + tile,
      mask = HandMask.add(mask, tile),
    )
  }

  def removeTile(tile: Domino): PossibleHand = {
    copy(
      hand = hand - tile,
      mask = HandMask.remove(mask, tile),
    )
  }

  def setCumulativeLikelihood(x: Double): PossibleHand = {
    copy(cumulativeLikelihood = x)
  }

  def contains(tile: Domino): Boolean = {
    HandMask.contains(mask, tile)
  }

  override def toString(): String = {
    s"${likelihood * 100}%: ${hand.mkString(",")}"
  }
}

object PossibleHand {
  def apply(
      hand: Set[Domino],
      likelihood: Double,
      cumulativeLikelihood: Double,
  ): PossibleHand = {
    new PossibleHand(
      hand,
      likelihood,
      cumulativeLikelihood,
      HandMask.fromHand(hand),
    )
  }
}
