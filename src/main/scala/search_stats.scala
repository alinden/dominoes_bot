package game

import java.util.concurrent.atomic.LongAdder

object SearchStats {
  private val nodes = new LongAdder()
  private val drawNodes = new LongAdder()
  private val drawSimulations = new LongAdder()

  def reset(): Unit = {
    nodes.reset()
    drawNodes.reset()
    drawSimulations.reset()
  }

  def visit(): Unit = nodes.increment()

  def visitDraw(simulations: Int): Unit = {
    drawNodes.increment()
    drawSimulations.add(simulations.toLong)
  }

  def count(): Long = nodes.sum()

  def drawCount(): Long = drawNodes.sum()

  def drawSimulationCount(): Long = drawSimulations.sum()
}
