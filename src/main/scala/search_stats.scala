package game

import java.util.concurrent.atomic.LongAdder

object SearchStats {
  private val nodes = new LongAdder()

  def reset(): Unit = nodes.reset()

  def visit(): Unit = nodes.increment()

  def count(): Long = nodes.sum()
}
