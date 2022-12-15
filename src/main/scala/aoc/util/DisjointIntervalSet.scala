package aoc.util

import cats.instances.order._

import scala.annotation.tailrec
import scala.collection.SortedSet

abstract case class DisjointIntervalSet private(underlying: SortedSet[Interval])
  extends Iterable[Int] {

  def contains(interval: Interval): Boolean =
    underlying.exists(_.contains(interval))

  def remove(interval: Interval): DisjointIntervalSet =
    new DisjointIntervalSet(underlying.flatMap(_.remove(interval))) {}

  def removeAll(intervals: DisjointIntervalSet): DisjointIntervalSet =
    intervals.underlying.foldLeft(this)(_.remove(_))

  override def size: Int = underlying.view.map(_.length).sum

  override def iterator: Iterator[Int] = underlying.iterator.flatMap(_.toRange.iterator)

  override def toString(): String = underlying.map(_.toString).mkString("[", ",", "]")
}

object DisjointIntervalSet {

  def from(intervals: Interval*): DisjointIntervalSet = {
    @tailrec
    def loop(remaining: List[Interval], acc: List[Interval]): List[Interval] =
      remaining match {
        case Nil => acc.reverse
        case head :: tail =>
          acc match {
            case Nil => loop(tail, head :: Nil)
            case head2 :: tail2 =>
              head.union(head2) match {
                case None => loop(tail, head :: acc)
                case Some(union) => loop(tail, union :: tail2)
              }
          }

      }

    new DisjointIntervalSet(SortedSet.from(loop(intervals.sorted.toList, Nil))) {}
  }
}


