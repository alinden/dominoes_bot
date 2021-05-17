package game

case class PlayedDomino(
    tile: Domino,
    value: Int,
) {

  def score: Int = {
    if (tile.isDouble()) tile.low + tile.high else value
  }

  override def toString(): String = {
    s"${tile} (${value})"
  }

  def connectTo(domino: Domino): Option[Int] = {
    // println(s"connectTo(${domino}) called on tile ${tile}")
    if (tile.isDouble()) {
      if (domino.high == this.tile.high) return Some(domino.low)
      if (domino.low == this.tile.high) return Some(domino.high)
    } else {
      if (domino.high == value) return Some(domino.low)
      if (domino.low == value) return Some(domino.high)
    }
    None
  }

  def backwards(): PlayedDomino = {
    if (this.tile.high == value) {
      PlayedDomino(this.tile, this.tile.low)
    } else {
      PlayedDomino(this.tile, this.tile.high)
    }
  }
}
