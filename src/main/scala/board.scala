package game

sealed trait BoardState
case object Empty extends BoardState
case object SpinnerOnly extends BoardState
case object Snake extends BoardState
case object Tee extends BoardState
case object Cross extends BoardState

case class Board(
    spinner: Option[PlayedDomino],
    left: List[PlayedDomino],
    right: List[PlayedDomino],
    up: List[PlayedDomino],
    down: List[PlayedDomino],
) {
  override def toString(): String = {
    List(
      s"spinner: ${spinner.map(_.tile).getOrElse("None")}",
      s"left: ${Board.formatChain(left).mkString(" - ")}",
      s"right: ${Board.formatChain(right).mkString(" - ")}",
      s"up: ${Board.formatChain(up).mkString(" - ")}",
      s"down: ${Board.formatChain(down).mkString(" - ")}",
      s"board score: ${score()}",
    ).mkString("\n")
  }

  def contains(domino: Domino): Boolean =
    tiles().exists { tile =>
      Domino(
        math.max(tile.high, tile.low),
        math.min(tile.high, tile.low)
      ) == domino
    }

  def length(): Int = spinner.size + left.size + right.size + up.size + down.size

  def isEmpty(): Boolean = {
    spinner.isEmpty && left.isEmpty && right.isEmpty && up.isEmpty && down.isEmpty
  }

  def state(): BoardState = {
    (spinner, left.headOption, right.headOption) match {
      case (None, None, None)          => Empty
      case (Some(_), None, None)       => SpinnerOnly
      case (None, Some(_), None)       => Snake
      case (Some(_), Some(_), None)    => Tee
      case (Some(_), Some(_), Some(_)) => Cross
      case (_, _, _) => {
        throw new Error(s"Corrupted board state: ${this}")
      }
    }
  }

  def tiles(): List[Domino] = {
    (spinner.toList ++ left ++ right ++ up ++ down).map(_.tile)
  }

  def score(): Int = {
    val lh = left.headOption.map(_.score).getOrElse(0)
    val rh = right.headOption.map(_.score).getOrElse(0)
    val uh = up.headOption.map(_.score).getOrElse(0)
    val dh = down.headOption.map(_.score).getOrElse(0)
    val sp = spinner.map(_.score).getOrElse(0)
    val ll = left.reverse.headOption.map(_.backwards().score).getOrElse(0)
    state() match {
      case Empty       => 0
      case SpinnerOnly => sp
      case Snake       => lh + ll
      case Tee         => sp + lh
      case Cross       => lh + rh + uh + dh
    }
  }

  /** Add a domino as the spinner.
    */
 def addSpinner(domino: Domino, direction: Direction): Option[Board] = {
    if (spinner.nonEmpty) return None

    val (connection, newLeft) = direction match {
      case Left =>
        (left.headOption, left.map(_.backwards()).reverse)

      case _ =>
        (left.lastOption.map(_.backwards()), left)
    }

    connection
      .flatMap(_.connectTo(domino))
      .map { _ =>
        copy(
          spinner = Some(PlayedDomino(domino, domino.high + domino.low)),
          left = newLeft,
          right = Nil,
          up = Nil,
          down = Nil
        )
      }
  }

  /** Add a domino to the left chain of the board.
    */
 def addLeft(domino: Domino): Option[Board] = {
    val connection = left.headOption.orElse(spinner)

    connection.flatMap(_.connectTo(domino)).map { value =>
      copy(left = PlayedDomino(domino, value) :: left)
    }
  }

  /** Add a domino to the right chain of the board. Or, in the case that there is no spinner yet,
    * add to the end of the left chain.
    */
 def addRight(domino: Domino): Option[Board] = {
    if (spinner.isEmpty) {
      // Before a spinner exists, extend the far end of the left chain.
      left.last.backwards().connectTo(domino).map { value =>
        copy(left = left :+ PlayedDomino(domino, value).backwards())
      }
    } else {
      // After a spinner exists, extend the right chain.
      val connection = right.headOption.orElse(spinner)

      connection.flatMap(_.connectTo(domino)).map { value =>
        copy(right = PlayedDomino(domino, value) :: right)
      }
    }
  }

  /** Add a domino to the up chain of the board.
    */
 def addUp(domino: Domino): Option[Board] = {
    val connection = up.headOption.orElse(spinner)

    connection.flatMap(_.connectTo(domino)).map { value =>
      copy(up = PlayedDomino(domino, value) :: up)
    }
  }

  /** Add a domino to the down chain of the board.
    */
  def addDown(domino: Domino): Option[Board] = {
    val connection = down.headOption.orElse(spinner)

    connection.flatMap(_.connectTo(domino)).map { value =>
      copy(down = PlayedDomino(domino, value) :: down)
    }
  }

  def addFirstTile(domino: Domino): Board =
    if (domino.isDouble())
      copy(spinner = Some(PlayedDomino(domino, domino.high + domino.low)))
    else
      copy(left = List(PlayedDomino(domino, domino.high)))
}

object Board {
  def empty(): Board = {
    Board(
      None,
      List(),
      List(),
      List(),
      List(),
    )
  }

  def formatChain(chain: List[PlayedDomino]): List[Domino] = {
    chain.map(pd => Domino(pd.value, pd.backwards().value))
  }
}
