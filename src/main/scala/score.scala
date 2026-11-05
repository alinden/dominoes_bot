package game

import cats.Monoid
import cats.syntax.foldable._
import cats.instances.list._

final case class Score(value: Double)

final case class ScoreAccumulator(
    sum: Double,
    sumSquares: Double,
    count: Int,
) {
  def normalized: Score =
    Score(sum / count)

  def standardError: Double =
    if (count < 2) Double.PositiveInfinity
    else {
      val n = count.toDouble
      val variance =
        math.max(0.0, (sumSquares - sum * sum / n) / (n - 1))

      math.sqrt(variance / n)
    }

  def estimate: ScoreEstimate =
    ScoreEstimate(normalized, standardError)
}

case class ScoreEstimate(
  mean: Score,
  standardError: Double,
)

object ScoreAccumulator {

  def fromScore(score: Score): ScoreAccumulator =
    ScoreAccumulator(score.value, score.value * score.value, 1)

  implicit val monoid: Monoid[ScoreAccumulator] =
    new Monoid[ScoreAccumulator] {
      def empty: ScoreAccumulator =
        ScoreAccumulator(0.0, 0.0, 0)

      def combine(
          x: ScoreAccumulator,
          y: ScoreAccumulator,
      ): ScoreAccumulator =
        ScoreAccumulator(
          x.sum + y.sum,
          x.sumSquares + y.sumSquares,
          x.count + y.count,
        )
    }

  def average(scores: Iterable[Score]): Score =
    scores.iterator
      .map(fromScore)
      .toList
      .combineAll
      .normalized

  def estimateAverage(scores: Iterable[Score]): ScoreEstimate =
    scores.iterator
      .map(fromScore)
      .toList
      .combineAll
      .estimate
}
