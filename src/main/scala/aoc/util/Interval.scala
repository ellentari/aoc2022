package aoc.util

import cats.Order
import cats.syntax.all._

case class Interval(start: Int, end: Int) {

  def toRange: Range = start to end

  def contains(other: Interval): Boolean =
    start <= other.start && other.end <= end

  def overlaps(other: Interval): Boolean =
    other.start <= end && start <= other.end

  def union(other: Interval): Option[Interval] =
    Option.when(overlaps(other))(Interval(start min other.start, end max other.end))

  def length: Int = end - start + 1

  def remove(interval: Interval): List[Interval] = {
    if (!overlaps(interval)) List(this)
    else if (interval.contains(this)) Nil
    else if (contains(interval))
      List(
        Interval(start, interval.start - 1),
        Interval(interval.end + 1, end)
      )
    else if (interval < this)
      List(Interval(interval.end + 1, end))
    else
      List(Interval(start, interval.start - 1))
  }

  def removeAll(set: DisjointIntervalSet): DisjointIntervalSet =
    set.underlying.foldLeft(DisjointIntervalSet.from(this))(_.remove(_))

  override def toString: String = s"[$start, $end]"
}

object Interval {

  implicit val orderInterval: Order[Interval] = Order.by(_.start)

}


