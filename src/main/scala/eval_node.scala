package game

trait EvalNode {
  def score(): Double;
  val depthRemaining: Int;
  val perspective: Perspective;
}

