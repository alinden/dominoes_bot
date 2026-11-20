package game

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.util.Random

object EngineReplayFixture {
  private def tileIds(tiles: List[Domino]): String =
    tiles.map(HandMask.tileIndex).mkString(",")

  private def actionCode(action: Action): String = action match {
    case Move(tile, direction) => s"${"LRUD"(Direction.all.indexOf(direction))}${HandMask.tileIndex(tile)}"
    case Draw => "D"
    case Pass => "P"
    case PlayHighestDouble => "H"
    case Reshuffle => throw new IllegalArgumentException("Use an explicit deal")
  }

  private def position(game: Game, phase: String, locked: Boolean = false): String = {
    val board = game.board
    val chains = List(board.spinner.toList, board.left, board.right, board.up, board.down)
    val layout = chains.map(_.map { placed =>
      val exposed = if (placed.tile.isDouble()) placed.tile.high else placed.value
      s"${HandMask.tileIndex(placed.tile)}:$exposed"
    }.mkString(",")).mkString("|")
    val (seat, moves, actions) = phase match {
      case "play" =>
        val actor = if (game.turn == PlayerOneTurn) "0" else "1"
        (actor, game.legalMoves().map(actionCode).mkString(","), game.allLegalActions().map(actionCode).mkString(","))
      case "opening" => ("-", "", "H")
      case _ => ("-", "", "")
    }
    val first = if (locked) "" else tileIds(game.playerOne.hand)
    val second = if (locked) "" else tileIds(game.playerTwo.hand)
    val winner = if (!game.isOver) "-" else if (game.playerOne.score > game.playerTwo.score) "0" else "1"
    s"S $phase $seat $winner ${game.playerOne.score}:${game.playerTwo.score} ${board.score()} $layout $first|$second|${tileIds(game.boneyard)} $moves|$actions"
  }

  private def replay(seed: Int): List[String] = {
    val random = new Random(seed)
    val trace = ListBuffer(s"MATCH $seed")
    var game = Game(Board.empty(), Player("first", Nil, 0), Player("second", Nil, 0), HighestDouble, Nil, GameLog.empty())
    var phase = "deal"
    var turns = 0
    while (!game.isOver) {
      turns += 1
      require(turns <= 2000, s"Match $seed did not finish")
      if (phase == "deal") {
        // Start one replay with all doubles in the boneyard to exercise a scripted redeal.
        val tiles = if (seed == 1 && turns == 1) Domino.all().sortBy(_.isDouble())
                    else random.shuffle(Domino.all())
        trace += s"DEAL ${tileIds(tiles)}"
        game = game.copy(board = Board.empty(), playerOne = game.playerOne.copy(hand = tiles.take(7)),
          playerTwo = game.playerTwo.copy(hand = tiles.slice(7, 14)), boneyard = tiles.drop(14),
          log = GameLog.empty(), lastTurnPass = false)
        phase = if (game.turn == HighestDouble) "opening" else "play"
        trace += position(game, phase)
      }

      val action = if (phase == "opening") PlayHighestDouble else {
        val actions = game.allLegalActions()
        actions(random.nextInt(actions.size))
      }
      trace += s"ACT ${actionCode(action)}"
      val before = game
      var locked = false
      var ended = false
      val (actor, placed) = action match {
        case PlayHighestDouble =>
          // The public no-arg opener shuffles on failure. Use the same per-tile rule with a scripted redeal.
          val opening = Domino.doubles().reverseIterator.flatMap { tile =>
            game.playHighestDouble(tile).map { case (next, _) => (next, tile) }
          }.take(1).toList.headOption
          opening match {
            case Some((next, tile)) =>
              game = next
              phase = "play"
              (Some(if (next.turn == PlayerOneTurn) 1 else 0), Some(Move(tile, Left)))
            case None =>
              phase = "deal"
              (None, None)
          }
        case _ =>
          val seat = if (game.turn == PlayerOneTurn) 0 else 1
          locked = action == Pass && game.lastTurnPass
          ended = locked || (action.isInstanceOf[Move] && game.activePlayer().hand.size == 1)
          game = game.act(action, debug = false, dontShuffle = true).get
          if (ended) phase = if (game.isOver) "done" else "deal"
          (Some(seat), action match {
            case move: Move => Some(move)
            case _ => None
          })
      }
      val playPoints = if (placed.nonEmpty && game.board.score() % 5 == 0) game.board.score() else 0
      val deltas = List(game.playerOne.score - before.playerOne.score, game.playerTwo.score - before.playerTwo.score)
        .zipWithIndex.map { case (delta, seat) => delta - (if (actor.contains(seat)) playPoints else 0) }
      val bonus = deltas.sum
      val recipient = if (bonus == 0) "-" else deltas.indexOf(bonus).toString
      val round = if (!ended) "-" else if (locked) "lock" else "out"
      val draw = if (action != Draw) "-" else {
        val entry = game.log.log.head.asInstanceOf[DrawLogEntry]
        s"${entry.playerHandSize}:${tileIds(entry.legalTilesToPlay)}"
      }
      trace += s"E ${actor.fold("-")(_.toString)} ${placed.fold("-")(actionCode)} $playPoints $round:$recipient:$bonus $draw"
      trace += position(game, phase, locked)
    }
    trace += "END"
    trace.toList
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 1, "Supply the output fixture path")
    val trace = (1 to 3).flatMap(replay).mkString("\n") + "\n"
    Files.writeString(Paths.get(args(0)), trace, StandardCharsets.UTF_8)
  }
}
