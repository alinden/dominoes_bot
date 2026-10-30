package game

object HandMask {

  // Unique index from 0 to 27 for each double-six domino.
  // Assumes Domino has fields high and low.
  def tileIndex(d: Domino): Int = {
    val high = math.max(d.high, d.low)
    val low = math.min(d.high, d.low)

    high * (high + 1) / 2 + low
  }

  def tileBit(d: Domino): Int =
    1 << tileIndex(d)

  def fromHand(hand: Iterable[Domino]): Int =
    hand.foldLeft(0) { (mask, domino) =>
      mask | tileBit(domino)
    }

  def contains(mask: Int, domino: Domino): Boolean =
    (mask & tileBit(domino)) != 0

  def add(mask: Int, domino: Domino): Int =
    mask | tileBit(domino)

  def remove(mask: Int, domino: Domino): Int =
    mask & ~tileBit(domino)

  def intersects(a: Int, b: Int): Boolean =
    (a & b) != 0

  def size(mask: Int): Int =
    Integer.bitCount(mask)
}
