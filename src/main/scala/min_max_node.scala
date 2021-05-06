package game

trait MinMaxNode {
  val numSimulations: Int;
  val depthRemaining: Int;
  val game: Game;
  val perspective: Perspective;
  def isBetterScore(candidate: Double, previousBest: Double, margin: Double = 0.0): Boolean;
  def getScoredMoves(limitToMoves: List[Action] = List()): List[(Double,Action)];

  def score(): Double = {
    bestScoredMove().map(_._1).getOrElse(0.toFloat)
  }
  def bestAction(limitToMoves: List[Action] = List()): (Double,Action) = {
    bestScoredMove(limitToMoves).getOrElse((0.0,Pass))
  }
  def goodActions(): List[Action] = {
    if (possibleFutures().length == 1) {
      List(possibleFutures().head._2)
    } else {
      val margin = 4.0
      val sortedMoves = getScoredMoves()
        .sortWith{ case ((xScore, _), (yScore, _)) => {
          isBetterScore(xScore, yScore)
        }}
      val bestScore = sortedMoves.head._1
      sortedMoves
        .withFilter{ case (score, move) => {
          isBetterScore(score, bestScore, margin)
        }}
        .map(_._2)
    }
  }
  def safeGame(): Game = {
    game.withReplacedActiveHand(perspective.hand)
  }
  def bestScoredMove(limitToMoves: List[Action] = List()): Option[(Double,Action)] = {
    getScoredMoves(limitToMoves).foldRight[Option[(Double,Action)]](None){ case ((score, move), z) => z match {
      case None => Some((score, move))
      case Some((zScore, zMove)) => if (isBetterScore(score, zScore)) Some((score, move)) else z
    }}
  }
  def possibleFutures(): List[(Game,Action)] = {
    val allFutures: List[(Option[Game],Action)] = safeGame().allLegalActions()
      .map((a) => (safeGame().act(a, false, true), a))
    val possibleFutures: List[(Game,Action)] = allFutures
      .withFilter{
        case (Some(g), Move(tile, _)) => {
          if (g.board.tiles().length != 1) {
            true
          } else {
            // First turn.
            // Remove the [6,3], [4,2], [3,4] and [6,2] since they're not good
            // starting moves and also any tiles that aren't connected
            // to any anothers since these are also bad.
            val badStartingPieces = Set(Domino(6,3),Domino(4,2),Domino(3,4),Domino(6,2))
            val hand = perspective.hand.toSet + tile
            val connectingPieces = Domino.all()
              .filter((d) => {
                d.high == tile.high || d.high == tile.low || d.low == tile.high || d.low == tile.low
              })
              .toSet
            (hand.intersect(connectingPieces) != Set(tile)) && (!badStartingPieces.contains(tile))
          }
        }
        case (Some(g), _) => true
        case (None, _) => false
      }
      .flatMap{
        case (Some(g), a) => List((g,a))
        case (None, _) => List()
      }
      possibleFutures
  }
}
