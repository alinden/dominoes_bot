package game

import scala.util.Random
import scala.io.StdIn

case class Game(
    board: Board,
    playerOne: Player,
    playerTwo: Player,
    turn: Turn,
    boneyard: List[Domino],
    log: GameLog,
    lastTurnPass: Boolean = false,
    isOver: Boolean = false,
) {
  override def toString(): String = {
    val showRobotTiles = false
    val robotHand = if (showRobotTiles) {
      s"${playerTwo.name}${if (activePlayer() == playerTwo) "*"
        else ""}(${playerTwo.score}): ${playerTwo.hand.mkString(" ")}"
    } else {
      s"${playerTwo.name}${if (activePlayer() == playerTwo) "*"
        else ""}(${playerTwo.score}): ${playerTwo.hand.map((_) => "[x,x]").mkString(" ")}"
    }
    (
      List(
        "",
        "----------------------------------",
        s"${playerOne.name}${if (activePlayer() == playerOne) "*"
          else ""}(${playerOne.score}): ${playerOne.hand.mkString(" ")}",
        robotHand,
        s"boneyard: ${boneyard.map((_) => "[x,x]").mkString(" ")}",
        s"tiles: ${boneyard.length + playerOne.hand.length + playerTwo.hand.length + board.length()}",
        board.toString(),
        "",
      ),
    ).mkString("\n")
  }

  def playHighestDouble(): Option[Game] = {
    val doubles = (6 to 0 by -1).map(n => Domino(n, n))

    val opening = doubles.iterator
      .flatMap { domino =>
        playHighestDouble(domino).map { case (game, message) =>
          (
            game.log(HighestDoubleEntry(game.inactivePlayer().name, domino)),
            message
          )
        }
      }
      .take(1)
      .toList
      .headOption

    opening match {
      case Some((game, message)) =>
        println(message)
        Some(game)

      case None =>
        val newGame = Game.setup(playerOne.name, playerTwo.name)
        println("game: nobody had a double. reshuffling.")

        Some(
          newGame.copy(
            playerOne = newGame.playerOne.copy(score = playerOne.score),
            playerTwo = newGame.playerTwo.copy(score = playerTwo.score)
          )
        )
    }
  }

  def playHighestDouble(domino: Domino): Option[(Game, String)] = {
    val dominoScore = domino.high + domino.low

    val openingPlayer =
      if (playerOne.hand.contains(domino)) Some(playerOne)
      else if (playerTwo.hand.contains(domino)) Some(playerTwo)
      else None

    openingPlayer.flatMap { player =>
      player.removeTile(domino).map { updatedPlayer =>
        val scoredPlayer = updatedPlayer.incrementScore(dominoScore)

        val nextGame =
          if (player == playerOne)
            copy(
              board = board.addFirstTile(domino),
              playerOne = scoredPlayer,
              turn = PlayerTwoTurn,
              lastTurnPass = false,
              isOver = false
            )
          else
            copy(
              board = board.addFirstTile(domino),
              playerTwo = scoredPlayer,
              turn = PlayerOneTurn,
              lastTurnPass = false,
              isOver = false
            )

        (nextGame, s"${player.name}: Play the $domino to start the game")
      }
    }
  }

  def playerByName(name: String): Player = {
    if (playerOne.name == name) playerOne else playerTwo
  }

  def otherPlayer(name: String): Player = {
    if (playerOne.name == name) playerTwo else playerOne
  }

  def reshuffle(): Game = {
    println("game: shuffling")

    val newGame = Game.setup("", "")

    copy(
      board = newGame.board,
      playerOne = playerOne.copy(hand = newGame.playerOne.hand),
      playerTwo = playerTwo.copy(hand = newGame.playerTwo.hand),
      boneyard = newGame.boneyard,
      log = GameLog.empty(),
      lastTurnPass = false,
      isOver = false
    )
  }

  private def chain(direction: Direction): List[PlayedDomino] =
    direction match {
      case Left  => board.left
      case Right => board.right
      case Up    => board.up
      case Down  => board.down
    }

  def simplify(actions: List[Action]): List[Action] = {
    val emptyDirections = Direction.all.filter(dir => chain(dir).isEmpty)
    def filterRedundant(
        xs: List[Action],
        extraRedundantDirs: List[Direction] = List(),
    ): List[Action] = {
      val dirsToRemove = emptyDirections.drop(1) ++ extraRedundantDirs
      xs.filterNot((action) =>
        action match {
          case Move(_, direction) => dirsToRemove.contains(direction)
          case _                  => false
        },
      )
    }
    board.state() match {
      case Empty => {
        actions.filter((action) =>
          action match {
            case Move(_, direction) => (direction == Left)
            case _                  => false
          },
        )
      }
      case SpinnerOnly => filterRedundant(actions)
      case Snake => {
        val nonEmptyChain = Direction.all
          .map(chain)
          .find(_.nonEmpty)
          .get
        if (nonEmptyChain.head.value == nonEmptyChain.last.backwards().value) {
          // Snake with equal valued head and tail.
          return actions.filterNot((action) =>
            action match {
              case Move(_, direction) => emptyDirections.contains(direction)
              case _                  => false
            },
          )
        } else {
          filterRedundant(actions)
        }
      }
      case Tee => filterRedundant(actions)
      case Cross => {
        val nonEmptyChainHeads: List[(Direction, PlayedDomino)] =
          Direction.all
            .flatMap { direction =>
              chain(direction).headOption.map(direction -> _)
            }
            .filterNot { case (_, played) =>
              played.tile.isDouble() && played.tile != Domino(0, 0)
            }
        if (nonEmptyChainHeads.isEmpty) {
          filterRedundant(actions)
        } else {
          val redundantDirections: List[Direction] =
            nonEmptyChainHeads
              .groupBy { case (_, played) => played.value }
              .values
              .toList
              .flatMap(_.drop(1).map(_._1))
          filterRedundant(actions, redundantDirections)
        }
      }
    }
  }

  private def showPass(debug: Boolean): Unit =
    if (debug) println(s"${activePlayer().name}: Pass")

  private def showLockedGame(debug: Boolean): Unit =
    if (debug) {
      println("")
      println("locked game")
      println("the next round will start with the highest double")
      println("")
    }

  private def showDominoes(game: Game, debug: Boolean): Unit =
    if (debug) {
      println("")
      println("game: dominoes!")
      println(game)
      println("")
    }

  def act(
    action: Action,
    debug: Boolean = true,
    dontShuffle: Boolean = false,
  ): Option[Game] = action match {
    case Pass =>
      showPass(debug)

      pass().flatMap { next =>
        if (lastTurnPass) {
          showLockedGame(debug)
          Game.endGame(next, debug, dontShuffle)
        } else {
          Some(next)
        }
      }

    case Draw =>
      draw().map { next =>
        if (debug) println(s"${activePlayer().name}: Draw")
        next
      }

    case Move(domino, direction) =>
      if (debug) println(s"${activePlayer().name}: Move the $domino $direction")

      move(domino, direction).flatMap { next =>
        if (next.inactivePlayer().hand.isEmpty) {
          showDominoes(next, debug)
          Game.endGame(next, debug, dontShuffle)
        } else {
          Some(next)
        }
      }

    case _ =>
      throw new Error(s"action: $action not supported by robot")
  }

  def pass(): Option[Game] =
    if (legalMoves().nonEmpty || boneyard.nonEmpty) {
      None
    } else {
      Some(
        copy(
          turn = incrementTurn(),
          boneyard = Nil,
          log = log.log(PassLogEntry(activePlayer().name)),
          lastTurnPass = true,
          isOver = false
        )
      )
    }

  def draw(): Option[Game] =
    if (legalMoves().nonEmpty || boneyard.isEmpty) {
      None
    } else {
      val tile = boneyard.head

      val (pOne, pTwo) =
        orderPlayers(activePlayer().addTile(tile), inactivePlayer())

      Some(
        copy(
          playerOne = pOne,
          playerTwo = pTwo,
          boneyard = boneyard.tail,
          log = log.log(
            DrawLogEntry(
              activePlayer().name,
              activePlayer().hand.length,
              legalTilesToPlay()
            )
          ),
          lastTurnPass = false,
          isOver = false
        )
      )
    }

  def log(entry: GameLogEntry): Game = copy(log = log.log(entry))

  def withReplacedActiveHand(hand: List[Domino]): Game = {
    val unavailable = board.tiles().toSet ++ hand
    val remainingTiles = Domino.all().filterNot(unavailable.contains)
    val shuffledTiles = Random.shuffle(remainingTiles)

    val newBoneyard = shuffledTiles.take(boneyard.length)
    val newOpponentHand = shuffledTiles.drop(boneyard.length)

    val (pOne, pTwo) = orderPlayers(
      activePlayer().copy(hand = hand),
      inactivePlayer().copy(hand = newOpponentHand)
    )

    copy(
      playerOne = pOne,
      playerTwo = pTwo,
      boneyard = newBoneyard
    )
  }

  def doFirstLegalAction(): Option[Game] = {
    allLegalActions().headOption.flatMap((action) => act(action))
  }

  def allLegalActions(): List[Action] = {
    val moves = legalMoves()

    if (moves.nonEmpty) {
      simplify(moves)
    } else if (boneyard.nonEmpty) {
      List(Draw)
    } else {
      List(Pass)
    }
  }

  def legalTilesToPlay(): List[Domino] =
    Game.allDirections()
      .flatMap { direction =>
        Domino.all().flatMap { domino =>
          updateBoard(domino, direction).map(_ => domino)
        }
      }
      .filterNot(board.contains)
      .distinct

  def scoringMoves(): List[(Move, Int)] =
    Game.allDirections()
      .flatMap { direction =>
        Domino.all().flatMap { domino =>
          updateBoard(domino, direction).map { updatedBoard =>
            (Move(domino, direction), updatedBoard.score())
          }
        }
      }
      .filterNot { case (move, score) =>
        board.contains(move.domino) || score % 5 != 0
      }
      .distinct
      .sortBy(_._2)
      .reverse

  def legalMoves(): List[Move] = {
    val hand = activePlayer().hand

    Game.allDirections().flatMap { direction =>
      hand.flatMap { domino =>
        updateBoard(domino, direction).map(_ => Move(domino, direction))
      }
    }
  }

  def move(
    domino: Domino,
    direction: Direction,
  ): Option[Game] =
    for {
      updatedBoard <- updateBoard(domino, direction)
      (pOne, pTwo) <- updatePlayers(domino, updatedBoard.score())
    } yield copy(
      board = updatedBoard,
      playerOne = pOne,
      playerTwo = pTwo,
      turn = incrementTurn(),
      log = log.log(
        MoveLogEntry(
          activePlayer().name,
          Move(domino, direction)
        )
      ),
      lastTurnPass = false,
      isOver = false
    )

  def getUserInput(): Option[Game] = {
    @annotation.tailrec
    def attempt(remaining: Int): Option[Game] = {
      if (remaining == 0) {
        None
      } else {
        getUserInputOnce() match {
          case Some(game) =>
            Some(game)

          case None =>
            println("error: illegal move")
            attempt(remaining - 1)
        }
      }
    }

    attempt(10)
  }

  def parseInput(input: String): Option[Action] = {
    val tokens = input.trim.split("\\s+").toList

    tokens match {
      case "d" :: _ =>
        Some(Draw)

      case "p" :: _ =>
        Some(Pass)

      case "h" :: _ =>
        printUserHelpText()
        None

      case "m" :: index :: direction :: _ =>
        val parsedDirection = direction.toLowerCase.headOption.flatMap {
          case 'l' => Some(Left)
          case 'r' => Some(Right)
          case 'u' => Some(Up)
          case 'd' => Some(Down)
          case _   => None
        }

        for {
          i <- index.toIntOption
          if i >= 0 && i < activePlayer().hand.length
          dir <- parsedDirection
        } yield Move(activePlayer().hand(i), dir)

      case _ =>
        None
    }
  }

  def printUserHelpText(): Unit = {
    println("Your turn")
    println("Tiles: ")
    println(
      activePlayer().hand.zipWithIndex
        .map { case (domino, i) => s"${i}: ${domino}" }
        .mkString("\n"),
    )
    println("To move your 3rd tile left, type 'm 2 l'")
    println("To draw, type 'd'")
    println("To pass, type 'p'")
  }

  def getUserInputOnce(): Option[Game] =
    Option(StdIn.readLine())
      .flatMap(parseInput)
      .flatMap(act(_))

  def incrementTurn(): Turn = turn match {
    case PlayerOneTurn => PlayerTwoTurn
    case PlayerTwoTurn => PlayerOneTurn
    case HighestDouble => HighestDouble
  }

  def activePlayer(): Player = turn match {
    case PlayerOneTurn => playerOne
    case PlayerTwoTurn => playerTwo
    case HighestDouble => playerOne
  }

  def inactivePlayer(): Player = turn match {
    case PlayerOneTurn => playerTwo
    case PlayerTwoTurn => playerOne
    case HighestDouble => playerTwo
  }

  def orderPlayers(
      active: Player,
      inactive: Player,
  ): (Player, Player) = turn match {
    case PlayerOneTurn => (active, inactive)
    case PlayerTwoTurn => (inactive, active)
    case HighestDouble => (active, inactive)
  }

  private def updatePlayers(
    domino: Domino,
    score: Int,
  ): Option[(Player, Player)] =
    activePlayer()
      .removeTile(domino)
      .map { player =>
        orderPlayers(player.incrementScore(score), inactivePlayer())
      }

  def forceMove(move: Move): Option[Game] = {
    updateBoard(move.domino, move.direction).map((b) => {
      Game( // 29 tile game
        b,
        playerOne,
        playerTwo,
        incrementTurn(),
        boneyard,
        log,
      )
    })
  }

  private def updateBoard(
      domino: Domino,
      direction: Direction,
  ): Option[Board] = (board.state(), direction) match {
    case (Empty, _)       => Some(board.addFirstTile(domino))
    case (SpinnerOnly, _) => board.addLeft(domino)
    case (Snake, Left) => {
      if (domino.isDouble()) {
        board.addSpinner(domino, Left)
      } else {
        board.addLeft(domino)
      }
    }
    case (Snake, Right) => {
      if (domino.isDouble()) {
        board.addSpinner(domino, Right)
      } else {
        board.addRight(domino)
      }
    }
    case (Tee, Left)    => board.addLeft(domino)
    case (Tee, Right)   => board.addRight(domino)
    case (Cross, Left)  => board.addLeft(domino)
    case (Cross, Right) => board.addRight(domino)
    case (Cross, Up)    => board.addUp(domino)
    case (Cross, Down)  => board.addDown(domino)
    case (_, _)         => None
  }
}

object Game {
  def setup(playerOneName: String, playerTwoName: String): Game = {
    val shuffledTiles = Random.shuffle(Domino.all());
    Game(
      Board.empty(),
      Player(playerOneName, shuffledTiles.slice(0, 7), 0),
      Player(playerTwoName, shuffledTiles.slice(7, 14), 0),
      HighestDouble,
      shuffledTiles.slice(14, 28),
      GameLog.empty(),
    )
  }

  def allDirections(): List[Direction] = List(Left, Right, Up, Down)

  def endGame(
      game: Game,
      debug: Boolean = true,
      dontShuffle: Boolean = false,
  ): Option[Game] = {

    val g = game.copy(
      turn = if (game.lastTurnPass) HighestDouble else game.incrementTurn(),
      isOver = matchIsOver(game.playerOne, game.playerTwo)
    )

    val activePlayer = g.activePlayer()
    val inactivePlayer = g.inactivePlayer()

    val activePips = pipTotal(activePlayer.hand)
    val inactivePips = pipTotal(inactivePlayer.hand)

    val scoredGame =
      if (g.lastTurnPass) {
        // Locked game: award the rounded difference in remaining pips.
        val difference = roundToFive(inactivePips - activePips)

        val updatedActive = activePlayer.copy(
          hand = Nil,
          score = activePlayer.score + math.max(difference, 0)
        )

        val updatedInactive = inactivePlayer.copy(
          hand = Nil,
          score = inactivePlayer.score + math.max(-difference, 0)
        )

        val (pOne, pTwo) = g.orderPlayers(updatedActive, updatedInactive)

        g.copy(
          playerOne = pOne,
          playerTwo = pTwo,
          boneyard = Nil,
          lastTurnPass = true,
          isOver = matchIsOver(pOne, pTwo)
        )

      } else {
        // Dominoes: award the opponent's remaining pips.
        val points = roundToFive(inactivePips)

        if (points > 0) {
          showRemainingTiles(inactivePlayer, debug)

          val updatedActive = activePlayer.copy(
            score = activePlayer.score + points
          )

          val (pOne, pTwo) = g.orderPlayers(updatedActive, inactivePlayer)

          g.copy(
            playerOne = pOne,
            playerTwo = pTwo,
            isOver = matchIsOver(pOne, pTwo)
          )
        } else {
          g
        }
      }

      if (scoredGame.isOver) {
        showMatchWinner(scoredGame, debug)
        Some(scoredGame)
      } else {
        Some(if (dontShuffle) scoredGame else scoredGame.reshuffle())
      }
  }

  private def pipTotal(hand: List[Domino]): Int =
    hand.map(d => d.high + d.low).sum

  private def roundToFive(value: Int): Int =
    math.round(value.toFloat / 5) * 5

  private def matchIsOver(playerOne: Player, playerTwo: Player): Boolean =
    (playerOne.score >= 150 || playerTwo.score >= 150) &&
      playerOne.score != playerTwo.score

  private def showRemainingTiles(player: Player, debug: Boolean): Unit =
    if (debug)
      println(s"game: ${player.name} had ${player.hand.mkString(" - ")}")

  private def showMatchWinner(game: Game, debug: Boolean): Unit =
    if (debug) {
      val winner =
        if (game.playerOne.score > game.playerTwo.score)
          game.playerOne
        else
          game.playerTwo

      println("Game Over!")
      println(s"The winner is: ${winner.name}")
    }
}
