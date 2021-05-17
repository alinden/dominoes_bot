package game

import cats.Semigroup

case class Score(
    values: List[Double]
) {
  def value: Double = values match {
    case Nil      => 0.0
    case h :: Nil => h
    case xs       => mean()
  }

  private def mean(): Double = values.sum / values.length

  private def variance(): Double = {
    val avg = mean()
    values.map((x) => math.pow((x - avg), 2)).sum / values.length
  }

  def stddev: Double = values match {
    case Nil      => 0.0
    case h :: Nil => 0.0
    case xs       => math.sqrt(variance())
  }

  def max(): Double = values.max
  def min(): Double = values.min

  def positiveLikelihood(current: Double): Double = {
    values.filter(_ > current).length.toFloat / values.length.toFloat
  }
}

object Score {
  def apply(value: Double): Score = Score(List(value))

  implicit val scoreSemigroup: Semigroup[Score] = new Semigroup[Score] {
    def combine(x: Score, y: Score): Score = {
      Score(x.values ++ y.values)
    }
  }
}
