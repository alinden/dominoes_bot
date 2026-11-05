package game

sealed trait MoveEvaluation
case object GoodMove extends MoveEvaluation
case object BadMove extends MoveEvaluation
case object ForcedMove extends MoveEvaluation

case class BayesObservation(
  hand: Set[Domino],
  evaluation: MoveEvaluation,
  preferredMoves: List[Move],
  scoredMoves: List[(Score, Move)]
)

