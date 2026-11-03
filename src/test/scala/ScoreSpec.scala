package game

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec

class ScoreSpec extends AnyFlatSpec {

  "ScoreAccumulator" should "average equally weighted scenarios" in {
    val scores = List(
      Score(10.0),
      Score(-10.0),
    )

    val result = ScoreAccumulator.average(scores)

    assert(result.value == 0.0)
  }

  it should "combine accumulators associatively" in {
    val a = ScoreAccumulator.fromScore(Score(2.0))
    val b = ScoreAccumulator.fromScore(Score(4.0))
    val c = ScoreAccumulator.fromScore(Score(9.0))

    val left = (a |+| b) |+| c
    val right = a |+| (b |+| c)

    assert(left == right)
    assert(left.normalized.value == 5.0)
  }

  it should "give nested scenarios equal weight" in {
    // First scenario contains ten equally weighted outcomes.
    val drawScenario =
      ScoreAccumulator.average(List.fill(10)(Score(10.0)))

    // Second scenario contains one outcome.
    val noDrawScenario = Score(-10.0)

    val result = ScoreAccumulator.average(
      List(drawScenario, noDrawScenario),
    )

    assert(result.value == 0.0)
  }
}
