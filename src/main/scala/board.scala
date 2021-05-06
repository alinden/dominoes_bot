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

  def contains(domino: Domino): Boolean = {
    (spinner.toList ++ left ++ right ++ up ++ down)
      .map{ case PlayedDomino(tile, value) => {
        Domino(math.max(tile.high, tile.low), math.min(tile.high, tile.low))
      }}
      .contains(domino)
  }

  def length(): Int = {
    (spinner.toList ++ left ++ right ++ up ++ down).length
  }

  def isEmpty(): Boolean = {
    spinner.isEmpty && left.isEmpty && right.isEmpty && up.isEmpty && down.isEmpty
  }

  def state(): BoardState = {
    (spinner, left.headOption, right.headOption) match {
      case (None, None, None) => Empty
      case (Some(_), None, None) => SpinnerOnly
      case (None, Some(_), None) => Snake
      case (Some(_), Some(_), None) => Tee
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
      case Empty => 0
      case SpinnerOnly => sp
      case Snake => lh + ll
      case Tee => sp + lh
      case Cross => lh + rh + uh + dh
    }
  }

  /**
   *  Add a domino as the spinner.
   */
  def addSpinner(domino: Domino, direction: Direction): Option[Board] = {
    if (!spinner.isEmpty) None
    val newSpinner = Some(PlayedDomino(domino, domino.high + domino.low))
    if (direction == Left) {
      left.headOption.flatMap((x) => {
        x.connectTo(domino).map((_) => {
          Board(
            newSpinner,
            this.left.map(_.backwards()).reverse,
            List(),
            List(),
            List(),
          )
        })
      })
    } else {
      left.reverse.headOption.map(_.backwards()).flatMap((x) => {
        x.connectTo(domino).map((_) => {
          Board(
            newSpinner,
            this.left,
            List(),
            List(),
            List(),
          )
        })
      })
    }
  }

  /**
   *  Add a domino to the left chain of the board.
   */
  def addLeft(domino: Domino): Option[Board] = {
    if (left.isEmpty) {
      spinner.flatMap((x) => {
        x.connectTo(domino).map((value) => {
          Board(
            this.spinner,
            List(PlayedDomino(domino, value)),
            this.right,
            this.up,
            this.down,
          )
        })
      })
    } else {
      this.left.head.connectTo(domino).map((value) => {
        Board(
          this.spinner,
          PlayedDomino(domino, value)::this.left,
          this.right,
          this.up,
          this.down,
        )
      })
    }
  }

  /**
   *  Add a domino to the right chain of the board.
   *  Or, in the case that there is no spinner yet,
   *  add to the end of the left chain.
   */
  def addRight(domino: Domino): Option[Board] = {
    if (right.isEmpty && spinner.isEmpty) {
      this.left.last.backwards().connectTo(domino).map((value) => {
        Board(
          this.spinner,
          this.left ++ List(PlayedDomino(domino, value).backwards()),
          this.right,
          this.up,
          this.down,
        )
      })
    } else if (right.isEmpty && !spinner.isEmpty) {
      return spinner.flatMap((x) => {
        x.connectTo(domino).map((value) => {
          Board(
            this.spinner,
            this.left,
            List(PlayedDomino(domino, value)),
            this.up,
            this.down,
          )
        })
      })
    } else {
      this.right.head.connectTo(domino).map((value) => {
        Board(
          this.spinner,
          this.left,
          PlayedDomino(domino, value)::this.right,
          this.up,
          this.down,
        )
      })
    }
  }

  /**
   *  Add a domino to the up chain of the board.
   */
  def addUp(domino: Domino): Option[Board] = {
    if (up.isEmpty) {
      spinner.flatMap((x) => {
        x.connectTo(domino).map((value) => {
          Board(
            this.spinner,
            this.left,
            this.right,
            List(PlayedDomino(domino, value)),
            this.down,
          )
        })
      })
    } else {
      this.up.head.connectTo(domino).map((value) => {
        Board(
          this.spinner,
          this.left,
          this.right,
          PlayedDomino(domino, value)::this.up,
          this.down,
        )
      })
    }
  }

  /**
   *  Add a domino to the down chain of the board.
   */
  def addDown(domino: Domino): Option[Board] = {
    if (down.isEmpty) {
      spinner.flatMap((x) => {
        x.connectTo(domino).map((value) => {
          Board(
            this.spinner,
            this.left,
            this.right,
            this.up,
            List(PlayedDomino(domino, value)),
          )
        })
      })
    } else {
      this.down.head.connectTo(domino).map((value) => {
        Board(
          this.spinner,
          this.left,
          this.right,
          this.up,
          PlayedDomino(domino, value)::this.down,
        )
      })
    }
  }

  def addFirstTile(domino: Domino): Board = {
    if (domino.isDouble()) {
      // First tile is a double.
      val value = domino.high + domino.low
      Board(Some(PlayedDomino(domino, value)), List(), List(), List(), List())
    } else {
      // First tile is not a double.
      // Which value to choose is arbitrary. Pick the high one.
      val value = domino.high
      Board(None, List(PlayedDomino(domino, value)), List(), List(), List())
    }
  }
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
    chain.map( pd => Domino(pd.value, pd.backwards().value))
  }
}
