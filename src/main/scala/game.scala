package game

import scala.util.Try
import scala.util.Random
import scala.io.StdIn
import java.util.Scanner

import cats.implicits._

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
    val showRobotTiles = true
    val robotHand = if (showRobotTiles) {
      s"${playerTwo.name}${if (activePlayer() == playerTwo) "*" else ""}(${playerTwo.score}): ${playerTwo.hand.mkString(" ")}"
    } else {
      s"${playerTwo.name}${if (activePlayer() == playerTwo) "*" else ""}(${playerTwo.score}): ${playerTwo.hand.map((_) => "[x,x]").mkString(" ")}"
    }
    (List(
      "",
      "----------------------------------",
      s"${playerOne.name}${if (activePlayer() == playerOne) "*" else ""}(${playerOne.score}): ${playerOne.hand.mkString(" ")}",
      robotHand,
      s"boneyard: ${boneyard.map((_) => "[x,x]").mkString(" ")}",
      s"tiles: ${boneyard.length + playerOne.hand.length + playerTwo.hand.length + board.length()}",
      board.toString(),
      "",
    )).mkString("\n")
  }

  def playHighestDouble(): Option[Game] = {
    val highestDoubleRank: List[Domino] = (0 to 6).toList
      .map((n) => Domino(n,n))
    highestDoubleRank
      .foldRight[Option[(Game,String)]](None){ (d, z) => {
        z.orElse(playHighestDouble(d).map{ case (afterGame, message) =>
          (
          afterGame.log(
            HighestDoubleEntry(
              afterGame.inactivePlayer().name,
              d,
            )
          )
          ,message)
        })
      }}
      .map{ case (game, message) => {
        println(message)
        game
      }}
      .orElse{
        val newGame = Game.setup(playerOne.name, playerTwo.name)
        println("game: nobody had a double. reshuffling.")
        Some(Game(
          newGame.board,
          Player(
            playerOne.name,
            newGame.playerOne.hand,
            playerOne.score,
          ),
          Player(
            playerTwo.name,
            newGame.playerTwo.hand,
            playerTwo.score,
          ),
          HighestDouble,
          newGame.boneyard,
          GameLog.empty(),
        ))
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
    Game(
      newGame.board,
      Player(
        playerOne.name,
        newGame.playerOne.hand,
        playerOne.score,
      ),
      Player(
        playerTwo.name,
        newGame.playerTwo.hand,
        playerTwo.score,
      ),
      turn,
      newGame.boneyard,
      GameLog.empty(),
    )
  }

  // TODO: generalize this to work for adding to a chain when
  // another chain has the same terminal value. Now it only keeps
  // one move onto an empty chain. It should also only allow one move
  // onto two chains of equal end value.
  def simplify(actions: List[Action]): List[Action] = {
    val emptyDirections = Direction.all.filter((dir) => dir match {
      case Left => board.left.isEmpty
      case Right => board.right.isEmpty
      case Up => board.up.isEmpty
      case Down => board.down.isEmpty
    })
    def filterRedundant(xs: List[Action], extraRedundantDirs: List[Direction] = List()): List[Action] = {
      if (emptyDirections.isEmpty) {
        xs
      } else {
        xs.filterNot((action) => action match {
          case Move(_, direction) => emptyDirections.tail.contains(direction) || extraRedundantDirs.contains(direction)
          case _ => false
        })
      }
    }
    board.state() match {
      case Empty => {
        actions.filter((action) => action match {
          case Move(_, direction) => (direction == Left)
          case _ => false
        })
      }
      case SpinnerOnly => filterRedundant(actions)
      case Snake => {
        val nonEmptyChain = Direction.all
          .filterNot((dir) =>
            emptyDirections.contains(dir)
          )
          .map((dir) => dir match {
            case Left => board.left
            case Right => board.right
            case Up => board.up
            case Down => board.down
          }).head
        if (nonEmptyChain.head.value == nonEmptyChain.last.backwards().value) {
          // Snake with equal valued head and tail.
          return actions.filterNot((action) => action match {
            case Move(_, direction) => emptyDirections.contains(direction)
            case _ => false
          })
        } else {
          filterRedundant(actions)
        }
      }
      case Tee => filterRedundant(actions)
      case Cross => {
        val nonEmptyChainHeads: List[(Direction, PlayedDomino)] = Direction.all
          .filterNot((dir) => dir match {
            case Left => board.left.isEmpty
            case Right => board.right.isEmpty
            case Up => board.up.isEmpty
            case Down => board.down.isEmpty
          }).map((dir) => dir match {
            case Left => (Left, board.left.head)
            case Right => (Right, board.right.head)
            case Up => (Up, board.up.head)
            case Down => (Down, board.down.head)
          }).filterNot((x) => {
            val tile = x._2.tile
            tile.isDouble() && (tile != Domino(0,0))
          })
        val redundantDirections: List[Direction] = nonEmptyChainHeads
          .map{ case (dir, PlayedDomino(_, v)) => Map(v -> List(dir)) }
          .reduce(_ |+| _) // will ++ dirs
          .toList
          .flatMap{ case (_, dirs) => dirs.drop(1) }
        filterRedundant(actions, redundantDirections)
      }
    }
  }

  def playHighestDouble(domino: Domino): Option[(Game,String)] = {
    var t: Turn = HighestDouble
    var message: String = ""
    var (pOne, pTwo) = (playerOne, playerTwo)
    val dominoScore = domino.high + domino.low
    if (playerOne.hand.contains(domino)) {
      t = PlayerTwoTurn
      val maybePOne = pOne.removeTile(domino).map((p) => p.incrementScore(dominoScore))
      if (maybePOne.isEmpty) return None // Should be impossible
      pOne = maybePOne.get
      message = s"${pOne.name}: Play the ${domino} to start the game"
    } else if (playerTwo.hand.contains(domino)) {
      t = PlayerOneTurn
      val maybePTwo = pTwo.removeTile(domino).map((p) => p.incrementScore(dominoScore))
      if (maybePTwo.isEmpty) return None // Should be impossible
      pTwo = maybePTwo.get
      message = s"${pTwo.name}: Play the ${domino} to start the game"
    } else {
      return None
    }
    Some((Game(
        board.addFirstTile(domino),
        pOne,
        pTwo,
        t,
        boneyard,
        log,
    ), message))
  }

  def act(
    action: Action,
    debug: Boolean = true,
    dontShuffle: Boolean = false,
  ): Option[Game] = action match {
    case Pass => {
      if (debug) {
        println(s"${activePlayer().name}: Pass")
      }
      pass().map((g) => {
        if (lastTurnPass) {
          if (debug) {
            println("")
            println("locked game")
            println("the next round will start with the highest double")
            println("")
          }
          return Game.endGame(g, debug, dontShuffle)
        }
        g
      })
    }
    case Draw => {
      draw().map((g) => {
        if (debug) {
          println(s"${activePlayer().name}: Draw")
        }
        g
      })
    }
    case Move(domino, direction) => {
      if (debug) {
        println(s"${activePlayer().name}: Move the $domino $direction")
      }
      move(domino, direction).map((g) => {
        if (g.inactivePlayer().hand.isEmpty) {
          if (debug) {
            println("")
            println("game: dominoes!")
            println(g)
            println("")
          }
          return Game.endGame(g, debug, dontShuffle)
        }
        g
      })
    }
    case _ => {
      throw new Error(s"action: ${action} not supported by robot")
    }
  }

  def pass(): Option[Game] = {
    if (!legalMoves().isEmpty) return None
    if (!boneyard.isEmpty) return None
    Some(
      Game(
        board,
        playerOne,
        playerTwo,
        incrementTurn(),
        List(),
        log.log(PassLogEntry(activePlayer().name)),
        true,
      )
    )
  }

  def draw(
    luckyNumber: Option[Int] = None,
    forceTile: Option[Domino] = None,
  ): Option[Game] = {
    if (!legalMoves().isEmpty || boneyard.isEmpty) {
      return None
    }
    val index = luckyNumber.getOrElse(0)
    val tileAtIndex = forceTile.getOrElse(boneyard(index))
    val (pOne, pTwo) = orderPlayers(activePlayer().addTile(tileAtIndex), inactivePlayer())
    Some(
      Game(
        board,
        pOne,
        pTwo,
        turn,
        boneyard.filterNot(_ == tileAtIndex),
        log.log(DrawLogEntry(
          activePlayer().name,
          activePlayer().hand.length,
          legalTilesToPlay(),
        )),
      )
    )
  }

  def log(entry: GameLogEntry): Game = {
    Game(
      board,
      playerOne,
      playerTwo,
      turn,
      boneyard,
      log.log(entry),
      lastTurnPass,
      isOver,
    )
  }

  def withReplacedActiveHand(hand: List[Domino]): Game = {
    val remainingTiles = Domino.all()
      .filterNot((d) => board.contains(d) || hand.contains(d))
    val shuffledTiles = Random.shuffle(remainingTiles)
    val newBoneyard = shuffledTiles.take(boneyard.length)
    val newPTwoHand = shuffledTiles.drop(boneyard.length)
    val (pOne, pTwo) = orderPlayers(
      Player(activePlayer().name, hand, activePlayer().score),
      Player(inactivePlayer().name, newPTwoHand, inactivePlayer().score),
    )
    val updated = Game(
      board,
      pOne,
      pTwo,
      turn,
      newBoneyard,
      log,
      lastTurnPass,
      isOver,
    )
    updated
  }

  def doFirstLegalAction(): Option[Game] = {
    allLegalActions().headOption.flatMap((action) => act(action))
  }

  def allLegalActions(): List[Action] = {
    val nonMoveActions = List(
        (Pass, pass()),
        (Draw, draw())
      )
      .filterNot(_._2.isEmpty)
      .map(_._1)
    simplify(legalMoves() ++ nonMoveActions)
  }

  def legalTilesToPlay(): List[Domino] = {
    Game.allDirections()
      .flatMap((direction) => {
        Domino.all().map((domino) => {
          (domino, updateBoard(domino, direction))
        })
      })
      .filterNot(_._2.isEmpty)
      .map(_._1)
      .filterNot(board.contains(_))
      .distinct
  }

  def scoringMoves(): List[(Move,Int)] = {
    Game.allDirections()
      .flatMap((direction) => {
        Domino.all().map((domino) => {
          val board = updateBoard(domino, direction)
          (Move(domino, direction), board.map(_.score()))
        })
      })
      .filterNot(_._2.isEmpty)
      .map{ case (move, someScore) => (move, someScore.get) }
      .filterNot{ case (move, score) => {
        board.contains(move.domino) || (score % 5 != 0)
      }}
      .distinct
      .sortBy(_._2)
      .reverse
  }

  def legalMoves(): List[Move] = {
    Game.allDirections()
      .flatMap((direction) => {
        activePlayer().hand.map((domino) => {
          (Move(domino, direction), move(domino, direction))
        })
      })
      .filterNot(_._2.isEmpty)
      .map(_._1)
  }

  def move(
    domino: Domino,
    direction: Direction,
  ): Option[Game] = {
    for {
      b <- updateBoard(domino, direction)
      (pOne, pTwo) <- updatePlayers(domino, b.score())
    } yield {
      Game(
        b,
        pOne,
        pTwo,
        incrementTurn(),
        boneyard,
        log.log(MoveLogEntry(
          activePlayer().name,
          Move(domino, direction),
        )),
      )
    }
  }

  def getUserInput(): Option[Game] = {
    val maxAttempts = 10
    var i = 0
    var game: Option[Game] = None
    while((i < maxAttempts) && game.isEmpty) {
      game = getUserInputOnce()
      if (game.isEmpty) {
        println("error: illegal move")
      }
      i = i + 1
    }
    game
  }

  def parseInput(input: String): Option[Action] = {
    if (input.isEmpty) return None
    if (input.take(1) == "d") return Some(Draw)
    if (input.take(1) == "p") return Some(Pass)
    if (input.take(1) == "h") {
      printUserHelpText()
      return None
    }
    if (input.take(1) != "m") return None
    if (activePlayer().hand.isEmpty) return None
    val scanner = new Scanner(input)
    val _ = scanner.next
    val index = scanner.nextInt
    val rawDirection = scanner.next
    if (rawDirection.isEmpty) return None
    val direction = rawDirection.take(1).toLowerCase() match {
      case "l" => Left
      case "r" => Right
      case "u" => Up
      case "d" => Down
      case _ => return None
    }
    if (index < 0) return None
    if (index >= activePlayer().hand.length) return None
    val domino = activePlayer().hand(index)
    Some(Move(domino, direction))
  }

  def printUserHelpText(): Unit = {
    println("Your turn")
    println("Tiles: ")
    println(
      activePlayer()
        .hand
        .zipWithIndex
        .map{ case (domino, i) => s"${i}: ${domino}"}
        .mkString("\n")
    )
    println("To move your 3rd tile, type 'm 3'")
    println("To draw, type 'd'")
    println("To pass, type 'p'")
  }

  def getUserInputOnce(): Option[Game] = {
    val input: String = StdIn.readLine()
    val action: Option[Action] = Try(parseInput(input)).toOption.flatten
    action.flatMap((a) => act(a))
  }

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
    active: Player, inactive: Player
  ): (Player, Player) = turn match {
    case PlayerOneTurn => (active, inactive)
    case PlayerTwoTurn => (inactive, active)
    case HighestDouble => (active, inactive)
  }

  private def updatePlayers(
    domino: Domino,
    score: Int,
  ): Option[(Player,Player)] = {
    activePlayer().removeTile(domino).map((ap) => {
      orderPlayers(
        ap.incrementScore(score),
        inactivePlayer(),
      )
    })
  }

  def forceMove(move: Move): Option[Game] = {
    updateBoard(move.domino, move.direction).map((b) => {
      Game( // 29 tile game
        b,
        playerOne,
        playerTwo,
        incrementTurn(),
        boneyard,
        log
      )
    })
  }

  private def updateBoard(
    domino: Domino,
    direction: Direction,
    ): Option[Board] = (board.state(), direction) match {
      case (Empty, _) =>  Some(board.addFirstTile(domino))
      case (SpinnerOnly, _) =>  board.addLeft(domino)
      case (Snake, Left) =>  {
        if (domino.isDouble()) {
          board.addSpinner(domino, Left)
        } else {
          board.addLeft(domino)
        }
      }
      case (Snake, Right) =>  {
        if (domino.isDouble()) {
          board.addSpinner(domino, Right)
        } else {
          board.addRight(domino)
        }
      }
      case (Tee, Left) => board.addLeft(domino)
      case (Tee, Right) => board.addRight(domino)
      case (Cross, Left) => board.addLeft(domino)
      case (Cross, Right) => board.addRight(domino)
      case (Cross, Up) => board.addUp(domino)
      case (Cross, Down) => board.addDown(domino)
      case (_, _) => None
    }
}

object Game {
  def setup(playerOneName: String, playerTwoName: String): Game = {
    val shuffledTiles = Random.shuffle(Domino.all());
    Game(
      Board.empty(),
      Player(playerOneName, shuffledTiles.slice(0,7), 0),
      Player(playerTwoName, shuffledTiles.slice(7,14), 0),
      HighestDouble,
      shuffledTiles.slice(14,28),
      GameLog.empty(),
    )
  }

  def allDirections(): List[Direction] = List(Left, Right, Up, Down)

  def endGame(
    game: Game,
    debug: Boolean = true,
    dontShuffle: Boolean = false,
  ): Option[Game] = {
    // println("endgame")
    // println("game")
    // println(game)
    val g = Game(
      game.board,
      game.playerOne,
      game.playerTwo,
      if (game.lastTurnPass) HighestDouble else game.incrementTurn(),
      game.boneyard,
      game.log,
      game.lastTurnPass,
      ((game.playerOne.score >= 150) || (game.playerTwo.score >= 150)) && (game.playerOne.score != game.playerTwo.score),
    )

    val rawLoserTileCount = g
      .inactivePlayer()
      .hand
      .foldRight(0){ (domino, z) =>  {
        domino.high + domino.low + z
      }}
      .toFloat
    // println(s"rawLoserTileCount: $rawLoserTileCount")
    val rawWinnerTileCount = g
      .activePlayer()
      .hand
      .foldRight(0){ (domino, z) =>  {
        domino.high + domino.low + z
      }}
      .toFloat
    // println(s"rawWinnerTileCount: $rawWinnerTileCount")
    val loserTileCount = Math.round(rawLoserTileCount/5)*5

    val withAddedPoints = if (g.lastTurnPass) {
      // locked game
      val winnerMinusLoserCount = math.round((rawLoserTileCount - rawWinnerTileCount)/5)*5
      val lockedGameWinnerAdd = math.max(winnerMinusLoserCount, 0)
      val lockedGameLoserAdd = math.max(-winnerMinusLoserCount, 0)
      val (pOne, pTwo) = g.orderPlayers(
        Player(
          g.activePlayer().name,
          List(),
          g.activePlayer().score + lockedGameWinnerAdd,
        ),
        Player(
          g.inactivePlayer().name,
          List(),
          g.inactivePlayer().score + lockedGameLoserAdd,
        ),
      )
      Some(
        Game(
          g.board,
          pOne,
          pTwo,
          g.turn,
          List(),
          g.log,
          true,
          ((pOne.score >= 150) || (pTwo.score >= 150)) && (pOne.score != pTwo.score),
        )
      )
    } else if (loserTileCount > 0) {
      // opponent has some tiles to score
      // println("opponent has some tiles to score")
      // println("couting loser tiles")
      if (debug) println(s"game: ${g.inactivePlayer().name} had ${g.inactivePlayer().hand.mkString(" - ")}")
      val ap = Player(
        g.activePlayer().name,
        g.activePlayer().hand,
        g.activePlayer().score + loserTileCount,
      )
      val (pOne, pTwo) = g.orderPlayers(ap, g.inactivePlayer())
      Some(
        Game(
          g.board,
          pOne,
          pTwo,
          g.turn,
          g.boneyard,
          g.log,
          g.lastTurnPass,
          ((pOne.score >= 150) || (pTwo.score >= 150)) && (pOne.score != pTwo.score),
        )
      )
    } else {
      // opponent has no tiles to score
      Some(g)
    }
    if (withAddedPoints.map(_.isOver).getOrElse(false)) {
      val pOneWon = withAddedPoints
        .map((g) => g.playerOne.score > g.playerTwo.score)
        .getOrElse(false)
      val winnerName = withAddedPoints
        .map((g) => if (pOneWon) g.playerOne.name else g.playerTwo.name)
        .getOrElse("Nobody")
      if (debug) {
        println("Game Over!")
        println(s"The winner is: ${winnerName}")
      }
      withAddedPoints
    } else {
      withAddedPoints.map((g) => if (dontShuffle) g else g.reshuffle())
    }
  }
}
