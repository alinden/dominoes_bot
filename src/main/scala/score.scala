package game

import cats.Monoid
import cats.syntax.foldable._
import cats.instances.list._

final case class Score(value: Double)

final case class ScoreAccumulator(
    sum: Double,
    count: Int
) {
  def normalized: Score =
    Score(sum / count)
}

object ScoreAccumulator {

  def fromScore(score: Score): ScoreAccumulator =
    ScoreAccumulator(score.value, 1)

  implicit val monoid: Monoid[ScoreAccumulator] =
    new Monoid[ScoreAccumulator] {
      def empty: ScoreAccumulator =
        ScoreAccumulator(0.0, 0)

      def combine(
          x: ScoreAccumulator,
          y: ScoreAccumulator
      ): ScoreAccumulator =
        ScoreAccumulator(
          x.sum + y.sum,
          x.count + y.count
        )
    }

  def average(scores: Iterable[Score]): Score =
    scores.iterator
      .map(fromScore)
      .toList
      .combineAll
      .normalized
}
