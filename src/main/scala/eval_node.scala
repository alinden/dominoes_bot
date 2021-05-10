package game

trait EvalNode {
  def score(): Score;
  val depthRemaining: Int;
  val perspective: Perspective;
}

