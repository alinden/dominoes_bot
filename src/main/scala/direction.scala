package game

sealed trait Direction
case object Left extends Direction
case object Right extends Direction
case object Up extends Direction
case object Down extends Direction

object Direction {
  def all: List[Direction] = List(Left, Right, Up, Down)
}

