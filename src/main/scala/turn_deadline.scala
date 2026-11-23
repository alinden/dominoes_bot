package game

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NoStackTrace

object TurnDeadline {
  case object Expired extends RuntimeException("Turn deadline expired") with NoStackTrace

  private val active = new ThreadLocal[Option[TurnDeadline]] {
    override def initialValue(): Option[TurnDeadline] = None
  }

  def after(duration: FiniteDuration): TurnDeadline =
    new TurnDeadline(System.nanoTime() + duration.toNanos)

  def current: Option[TurnDeadline] = active.get()

  def withCurrent[A](deadline: Option[TurnDeadline])(work: => A): A = {
    val previous = current
    active.set(deadline)
    try work
    finally active.set(previous)
  }

  def check(): Unit = current.foreach(_.check())

  def attempt[A](work: => A): Option[A] =
    try Some(work)
    catch { case Expired => None }
}

final class TurnDeadline private (endNanos: Long) {
  def check(): Unit =
    if (System.nanoTime() - endNanos >= 0) throw TurnDeadline.Expired
}
