package game

sealed trait Direction
case object Left extends Direction {
  override def toString(): String = "Left "
}
case object Right extends Direction
case object Up extends Direction {
  override def toString(): String = "Up   "
}
case object Down extends Direction {
  override def toString(): String = "Down "
}

object Direction {
  def all: List[Direction] = List(Left, Right, Up, Down)
}

