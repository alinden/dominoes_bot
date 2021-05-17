package game

sealed trait Turn
case object PlayerOneTurn extends Turn
case object PlayerTwoTurn extends Turn
case object HighestDouble extends Turn
