package game

case class Perspective(
    name: String,
    opponentName: String,
    hand: List[Domino],
    maybeOpponentHand: Option[List[Domino]],
    opponentHand: EliminationHand,
    opponentIdeaOfMyHand: Option[EliminationHand],
) {

  /** Before swapping perspectives with a random hand opponent, assume they have a specific hand.
    */
 def assumeOpponentHand(
    assumedHand: List[Domino],
    boardTiles: List[Domino],
  ): Perspective =
    copy(
      maybeOpponentHand = Some(assumedHand),
      opponentIdeaOfMyHand = opponentIdeaOfMyHand.orElse(
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
  *
  * Requires assumeOpponentHand() to have been called first.
  */
  def swap(): Perspective =
    copy(
      name = opponentName,
      opponentName = name,
      hand = maybeOpponentHand.get,
      maybeOpponentHand = Some(hand),
      opponentHand = opponentIdeaOfMyHand.get,
      opponentIdeaOfMyHand = Some(opponentHand)
    )

  /** Update with a new game state. Assumes that the previous game states were already incorporated
    * in the unknown hands.
    */
 def update(before: Game, after: Game): Perspective = {
    val entry = after.log.log.head

    copy(
      hand = after.playerByName(name).hand,
      opponentHand = opponentHand.quickUpdateForEntry(entry, before),
      opponentIdeaOfMyHand =
        opponentIdeaOfMyHand.map(_.quickUpdateForEntry(entry, before))
    )
  }
}
