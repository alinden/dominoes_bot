package game

case class Player(
  name: String,
  hand: List[Domino],
  score: Int,
) {
  def removeTile(domino: Domino): Option[Player] = {
    val filteredHand = hand.filterNot(_ == domino)
    val playerHasDomino = filteredHand.length != hand.length
    if (!playerHasDomino) return None
    Some(Player(name, filteredHand, score))
  }

  def addTile(domino: Domino): Player = {
    Player(name, domino::hand, score)
  }

  def incrementScore(amount: Int): Player = {
    if (amount % 5 == 0) {
      Player(name, hand, score + amount)
    } else {
      Player(name, hand, score)
    }
  }

  override def toString(): String = {
    s"${name}: ${hand.mkString(" - ")}"
  }
}
