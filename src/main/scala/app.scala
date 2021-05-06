import game._

case class MatchScore(pOneScore: Int, pTwoScore: Int, bestOf: Int) {
  def isOver: Boolean = {
    val halfPlusOne = math.floor(bestOf.toFloat/2.0) + 1
    (pOneScore >= halfPlusOne) || (pTwoScore >= halfPlusOne)
  }
}

object App {
  def playGame(mode: String, matchScore: MatchScore): MatchScore = {
    val debug = (mode == "--play_greedy")
    var g: Option[Game] = Some(Game.setup("human", "robot"))
    var action: Action = Reshuffle
    println("New Game!")
    println(s"Playing a best of ${matchScore.bestOf}")
    println("")
    println(g.get)
    if (!debug) Thread.sleep(2000)
    var t = g
    var guess: Option[EliminationHand] = None
    while(!g.map(_.isOver).getOrElse(true)) {
      if (g.get.turn == HighestDouble) {
        println("")
        println("First Turn, playing the highest double.")
        if (!debug) Thread.sleep(2000)
        g = g.get.playHighestDouble()
        g = g.orElse(Some(t.get.reshuffle()))
        if (!g.isEmpty &&
          g.get.log.log.headOption.map(_.action == PlayHighestDouble).getOrElse(false)) {
          // highest double played
          guess = guess
            .orElse{
              Some(EliminationHand.createFromOpponentHand("human", g.get.playerTwo.hand))
            }
            .map(_.updateForEntry(g.get.log.log.head, t.get))
        }
      } else if (g.get.turn == PlayerOneTurn) {
        println("")
        println(g.get)
        g = if (mode == "--play_human") {
          g.get.getUserInput()
        } else {
          g.get.act(Robot.greedyMove(g.get).get, true)
        }
        if (!g.get.log.log.isEmpty) {
          guess = guess
            .orElse{
              Some(EliminationHand.createFromOpponentHand("human", g.get.playerTwo.hand))
            }
            .map(_.updateForEntry(g.get.log.log.head, t.get, false, true))
        } else {
          guess = None
        }
        if (debug) {
          println("--guess--")
          println(guess.getOrElse("no guess"))
          println("--guess--")
        }
     } else {
        action = Robot.act(g.get, guess.getOrElse(EliminationHand.createFromOpponentHand("human", g.get.playerTwo.hand)), debug).get
        g = g.get.act(action, true)
        if (!g.get.log.log.isEmpty) {
          guess = guess
            .orElse{
              Some(EliminationHand.createFromOpponentHand("human", g.get.playerTwo.hand))
            }
            .map(_.updateForEntry(g.get.log.log.head, t.get))
          if (g.get.log.log.head.action == Draw) {
            guess = guess.map(_.eliminateTile(g.get.playerTwo.hand.head))
          }
        } else {
          guess = None
        }
        if (debug) {
          println("--guess--")
          println(guess.getOrElse("no guess"))
          println("--guess--")
        }
      }
      t.flatMap((tt) => {
        g.map((gg) => {
          if (tt.playerOne.score < gg.playerOne.score) {
            val points = gg.playerOne.score - tt.playerOne.score
            println(s"${tt.playerOne.name}: score ${points} points")
          } else if (tt.playerTwo.score < gg.playerTwo.score) {
            val points = gg.playerTwo.score - tt.playerTwo.score
            println(s"${tt.playerTwo.name}: score ${points} points")
          }
        })
      })
      t = g
      println("")
    }
    println("game over, result:")
    println(g.getOrElse("None"))
    if (g.get.playerOne.score > g.get.playerTwo.score) {
      MatchScore(
        matchScore.pOneScore + 1,
        matchScore.pTwoScore,
        matchScore.bestOf,
      )
    } else {
      MatchScore(
        matchScore.pOneScore,
        matchScore.pTwoScore + 1,
        matchScore.bestOf,
      )
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("""Usage: sbt "run [--play_human|--play_greedy]"""")
      return
    }
    val mode = args.head
    val bestOf = if (mode == "--play_human") 3 else 19
    var matchScore = MatchScore(0,0,bestOf)
    while(!matchScore.isOver) {
      matchScore = playGame(mode, matchScore)
      println(matchScore)
    }
  }
}
