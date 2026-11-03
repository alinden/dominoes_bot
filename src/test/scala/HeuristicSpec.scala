package game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeuristicSpec extends AnyFlatSpec with Matchers {

  import HeuristicMode._

  def evaluate(
      robotScore: Double,
      humanScore: Double,
      gameOver: Boolean = false,
      dominoesWinner: Option[String] = None,
      mode: HeuristicMode = BoundedRunway
  ): Double =
    Heuristic.evaluate(
      robotScore,
      humanScore,
      gameOver,
      dominoesWinner,
      mode
    ).value

  "The heuristic" should "return zero for equal scores" in {
    evaluate(75, 75) shouldBe 0.0
    evaluate(120, 120) shouldBe 0.0
  }

  it should "prefer a lead closer to the target score" in {
    evaluate(120, 80) should be > evaluate(40, 0)
  }

  it should "prefer a smaller deficit closer to the start of the game" in {
    evaluate(0, 40) should be > evaluate(80, 120)
  }

  it should "be symmetric between players" in {
    val robotLeading = evaluate(120, 80)
    val humanLeading = evaluate(80, 120)

    robotLeading shouldBe (-humanLeading +- 0.000001)
  }

  it should "increase as the robot's lead increases" in {
    evaluate(100, 70) should be > evaluate(100, 80)
    evaluate(100, 80) should be > evaluate(100, 90)
  }

  it should "handle scores above the target without treating them as terminal" in {
    val value = evaluate(155, 140)

    value should be > 0.0
    value should be < 1.0
  }

  it should "return positive one for a completed robot victory" in {
    evaluate(155, 140, gameOver = true) shouldBe 1.0
  }

  it should "return negative one for a completed human victory" in {
    evaluate(140, 155, gameOver = true) shouldBe -1.0
  }

  it should "return zero for a completed tie" in {
    evaluate(150, 150, gameOver = true) shouldBe 0.0
  }

  it should "favor the player who dominoed" in {
    val neutral = evaluate(100, 80)

    val robotDominoed =
      evaluate(100, 80, dominoesWinner = Some("robot"))

    val humanDominoed =
      evaluate(100, 80, dominoesWinner = Some("human"))

    robotDominoed should be > neutral
    humanDominoed should be < neutral
  }

  it should "ignore dominoes bonuses when the game is over" in {
    evaluate(
      155,
      140,
      gameOver = true,
      dominoesWinner = Some("human")
    ) shouldBe 1.0
  }

  "The bounded runway heuristic" should "always return values between -1 and 1 for ongoing games" in {
    val scores = Seq(
      (0.0, 0.0),
      (40.0, 0.0),
      (120.0, 80.0),
      (149.0, 100.0),
      (155.0, 140.0),
      (200.0, 100.0),
      (0.0, 200.0)
    )

    scores.foreach { case (robot, human) =>
      val value = evaluate(robot, human)

      value should be > -1.0
      value should be < 1.0
    }
  }

  "The linear runway heuristic" should "calculate the expected score advantage" in {
    val value = evaluate(
      120,
      80,
      mode = LinearRunway
    )

    value shouldBe (40.0 / 30.0 +- 0.000001)
  }

  it should "allow values greater than one" in {
    evaluate(
      140,
      100,
      mode = LinearRunway
    ) should be > 1.0
  }

  "The bounded runway heuristic" should "preserve the ordering of the linear heuristic" in {
    val positions = Seq(
      (40.0, 0.0),
      (80.0, 40.0),
      (100.0, 60.0),
      (120.0, 80.0),
      (140.0, 100.0)
    )

    val linear = positions.map { case (r, h) =>
      evaluate(r, h, mode = LinearRunway)
    }

    val bounded = positions.map { case (r, h) =>
      evaluate(r, h, mode = BoundedRunway)
    }

    linear.sliding(2).foreach {
      case Seq(a, b) => b should be >= a
      case _         =>
    }

    bounded.sliding(2).foreach {
      case Seq(a, b) => b should be >= a
      case _         =>
    }
  }
}
