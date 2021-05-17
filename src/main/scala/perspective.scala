package game

case class Perspective(
    name: String,
    opponentName: String,
    hand: List[Domino],
    maybeOpponentHand: Option[List[Domino]],
    opponentHand: EliminationHand,
    opponentIdeaOfMyHand: Option[EliminationHand]
) {

  /** Before swapping perspectives with a random hand opponent, assume they have
    * a specific hand.
    */
  def assumeOpponentHand(
      assumedHand: List[Domino],
      boardTiles: List[Domino]
  ): Perspective = Perspective(
    name,
    opponentName,
    hand,
    Some(assumedHand),
    opponentHand,
    opponentIdeaOfMyHand.orElse(
      Some(
        EliminationHand.createFromOpponentHand(
          name,
          assumedHand ++ boardTiles,
          hand.length
        )
      )
    )
  )

  /** Swap perspectives from one player to the other.
    */
  def swap(): Perspective = {
    Perspective(
      opponentName,
      name,
      maybeOpponentHand.get,
      Some(hand),
      opponentIdeaOfMyHand.get,
      Some(opponentHand)
    )
  }

  /** Update with a new game state. Assumes that the previous game states were
    * already incorporated in the unknown hands.
    */
  def update(before: Game, after: Game): Perspective = {
    val entry = after.log.log.head
    Perspective(
      name,
      opponentName,
      after.playerByName(name).hand,
      maybeOpponentHand,
      opponentHand.quickUpdateForEntry(entry, before),
      opponentIdeaOfMyHand.map(_.quickUpdateForEntry(entry, before))
    )
  }
}
