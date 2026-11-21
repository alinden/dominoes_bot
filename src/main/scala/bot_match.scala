import game._

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

final class CppMatchBot(binary: String, seat: Int, val depth: Int, seed: Int, log: Path,
    searchSeconds: Int = 0)
    extends AutoCloseable {
  private val process = new ProcessBuilder(binary, seat.toString, depth.toString,
    Integer.toUnsignedString(seed), log.toString, searchSeconds.toString)
    .redirectError(ProcessBuilder.Redirect.INHERIT).start()
  private val input = new BufferedWriter(new OutputStreamWriter(process.getOutputStream, UTF_8))
  private val output = new BufferedReader(new InputStreamReader(process.getInputStream, UTF_8))

  private val directions = Map("L" -> Left, "R" -> Right, "U" -> Up, "D" -> Down)

  private def request(command: String): String = {
    input.write(command)
    input.newLine()
    input.flush()
    Option(output.readLine()).getOrElse(throw new IllegalStateException(s"C++ bot stopped at $command"))
  }

  private def check(command: String, game: Game): Unit = {
    val expected = s"OK ${game.playerOne.score} ${game.playerTwo.score}"
    val reply = request(command)
    require(reply == expected, s"C++ replay differs after $command: expected '$expected', got '$reply'")
  }

  def deal(tiles: List[Domino], game: Game): Unit =
    check(tiles.map(HandMask.tileIndex).mkString("DEAL ", " ", ""), game)

  def advance(action: Action, game: Game): Unit = {
    val encoded = action match {
      case Move(tile, direction) =>
        s"${"LRUD"(Direction.all.indexOf(direction))} ${HandMask.tileIndex(tile)}"
      case Draw => "W"
      case Pass => "P"
      case PlayHighestDouble => "H"
      case Reshuffle => throw new IllegalArgumentException("Supply the next deal explicitly")
    }
    check(s"ACT $encoded", game)
  }

  def choose(turn: Int): Action = request(s"CHOOSE $turn").split(" ").toList match {
    case "W" :: Nil => Draw
    case "P" :: Nil => Pass
    case dir :: id :: Nil if directions.contains(dir) => Move(Domino.all()(id.toInt), directions(dir))
    case response => throw new IllegalStateException(s"Invalid C++ move: ${response.mkString(" ")}")
  }

  override def close(): Unit = {
    try input.close()
    finally {
      if (!process.waitFor(2, TimeUnit.SECONDS)) {
        process.destroyForcibly()
        process.waitFor()
      }
      output.close()
    }
  }
}

object BotMatch {
  def main(args: Array[String]): Unit = {
    val settings = for {
      depth <- args.lift(1).getOrElse("15").toIntOption.filter(_ > 0)
      inference <- args.lift(2).getOrElse("30").toIntOption.filter(_ > 0)
      search <- args.lift(3).getOrElse("30").toIntOption.filter(_ > 0)
      pairs <- args.lift(4).getOrElse("1").toIntOption.filter(_ > 0)
      seed <- args.lift(5).getOrElse("42").toIntOption
      binary <- args.headOption if args.length <= 7
    } yield (binary, depth, inference, search, pairs, seed)

    val (binary, depth, inference, search, pairs, seed) = settings.getOrElse {
      println("Usage: runMain BotMatch CPP_BINARY [DEPTH [INFERENCE_SECONDS [SEARCH_SECONDS [PAIRS [SEED [LOG_DIR]]]]]]")
      return
    }

    val base = Files.createDirectories(Paths.get(args.lift(6).getOrElse("target/bot-match-logs")))
    val logDir = Files.createTempDirectory(base, "match-").toAbsolutePath
    val cppLog = Files.createFile(logDir.resolve("cpp-decisions.log"))
    val scalaLog = logDir.resolve("scala-decisions.log")
    val decisionLog = new PrintWriter(Files.newBufferedWriter(scalaLog, UTF_8))
    println(s"C++ decision log   $cppLog")
    println(s"Scala decision log $scalaLog")
    var cppWins = 0
    var scalaWins = 0
    try for (pair <- 0 until pairs; cppSeat <- 0 to 1) {
      val dealSeed = seed + pair
      val game = s"Pair ${pair + 1} | deal seed $dealSeed | C++ seat ${cppSeat + 1}, depth $depth | " +
        s"Scala inference ${inference}s, search ${search}s"
      println(game)
      decisionLog.println(game)
      decisionLog.flush()
      val cpp = new CppMatchBot(binary, cppSeat, depth, dealSeed, cppLog)
      val (cppScore, scalaScore) =
        try App.playCppGame(cpp, cppSeat, dealSeed, search, inference, Some(decisionLog))
        finally cpp.close()
      if (cppScore > scalaScore) cppWins += 1 else scalaWins += 1
      println(s"Result: C++ $cppScore / Scala $scalaScore | Wins: C++ $cppWins / Scala $scalaWins")
    } finally decisionLog.close()
  }
}

object CppDepthMatch {
  def main(args: Array[String]): Unit = {
    val settings = for {
      depthA <- args.lift(1).getOrElse("15").toIntOption.filter(_ > 0)
      depthB <- args.lift(2).getOrElse("68").toIntOption.filter(_ > 0)
      seconds <- args.lift(3).getOrElse("60").toIntOption.filter(_ > 0)
      pairs <- args.lift(4).getOrElse("1").toIntOption.filter(_ > 0)
      seed <- args.lift(5).getOrElse("48").toIntOption
      binary <- args.headOption if args.length <= 7 && depthA != depthB
    } yield (binary, depthA, depthB, seconds, pairs, seed)
    val (binary, depthA, depthB, seconds, pairs, seed) = settings.getOrElse {
      println("Usage: runMain CppDepthMatch CPP_BINARY [DEPTH_A [DEPTH_B [SECONDS [PAIRS [SEED [LOG_DIR]]]]]]")
      return
    }

    val base = Files.createDirectories(Paths.get(args.lift(6).getOrElse("target/bot-match-logs")))
    val logDir = Files.createTempDirectory(base, "cpp-depth-").toAbsolutePath
    val logA = Files.createFile(logDir.resolve(s"depth-$depthA.log"))
    val logB = Files.createFile(logDir.resolve(s"depth-$depthB.log"))
    println(s"Depth $depthA decision log $logA")
    println(s"Depth $depthB decision log $logB")

    var winsA = 0
    var winsB = 0
    for (pair <- 0 until pairs; seatA <- 0 to 1) {
      val dealSeed = seed + pair
      println(s"Pair ${pair + 1} | deal seed $dealSeed | depth $depthA seat ${seatA + 1} | " +
        s"depth $depthB seat ${2 - seatA} | $seconds seconds per search")
      val a = new CppMatchBot(binary, seatA, depthA, dealSeed, logA, seconds)
      val (scoreA, scoreB) = try {
        val b = new CppMatchBot(binary, 1 - seatA, depthB, dealSeed, logB, seconds)
        try App.playCppGame(a, seatA, dealSeed, 0, 0, None, Some(b))
        finally b.close()
      } finally a.close()
      if (scoreA > scoreB) winsA += 1 else winsB += 1
      println(s"Result: depth $depthA $scoreA / depth $depthB $scoreB | Wins: $winsA–$winsB")
    }
  }
}
