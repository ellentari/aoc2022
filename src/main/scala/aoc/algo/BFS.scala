package aoc.algo

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object BFS {

  def shortestPathLength[A](start: A)(next: A => List[A], isEnd: A => Boolean): Option[Int] =
    shortestPath(start)(next, isEnd).map(_.length - 1)

  def shortestPath[A](start: A)(next: A => List[A], isEnd: A => Boolean): Option[List[A]] = {

    @tailrec
    def loop(queue: Queue[A], parent: Map[A, A], seen: Set[A]): Option[List[A]] =
      queue.dequeueOption match {
        case None => None
        case Some((a, tail)) =>

          if (isEnd(a))Some(restorePath(a, parent.get))
          else {
            val toVisit = next(a).filterNot(seen.contains)

            loop(tail ++ toVisit, parent ++ toVisit.map(_ -> a), seen ++ toVisit)
          }
      }

    loop(Queue(start), Map.empty, Set(start))
  }

  def discoverRegion[A](start: A)(next: A => List[A]): Set[A] = {
    @tailrec
    def loop(queue: Queue[A], seen: Set[A]): Set[A] =
      queue.dequeueOption match {
        case None => seen
        case Some((a, tail)) =>
          val toVisit = next(a).filterNot(seen.contains)

          loop(tail ++ toVisit, seen ++ toVisit)
      }

    loop(Queue(start), Set(start))
  }

  def visitAll[A](start: A)(next: A => List[A], onVisit: A => Unit): Unit = {
    @tailrec
    def loop(queue: Queue[A], seen: Set[A]): Unit =
      queue.dequeueOption match {
        case None => ()
        case Some((a, tail)) =>
          onVisit(a)

          val toVisit = next(a).filterNot(seen.contains)

          loop(tail ++ toVisit, seen ++ toVisit)
      }

    loop(Queue(start), Set(start))
  }

}
