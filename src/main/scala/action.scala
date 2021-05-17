package game

sealed trait Action
case object Reshuffle extends Action
case object PlayHighestDouble extends Action
case object Pass extends Action
case object Draw extends Action
case class Move(domino: Domino, direction: Direction) extends Action
