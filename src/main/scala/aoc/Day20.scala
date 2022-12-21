package aoc

import scala.collection.mutable

object Day20 extends App {

  case class CircularVector[A](underlying: Vector[A]) extends AnyVal {

    def map[B](f: A => B): CircularVector[B] = CircularVector(underlying.map(f))
    def zipWithIndex: CircularVector[(A, Int)] = CircularVector(underlying.zipWithIndex)

    def get(i: Int): A = underlying(i % underlying.size)

    def move(i: Int, positions: Long): CircularVector[A] = {
      if ((positions % (underlying.size - 1)) == 0) this
      else {
        val toMove = underlying(i)
        val newIndex = indexToMoveTo(i, positions)

        val buf = mutable.ListBuffer.from[A](underlying)
        buf.remove(i)
        buf.insert(newIndex, toMove)

        CircularVector(buf.toVector)
      }
    }

    private def indexToMoveTo(i: Int, positions: Long) = {
      val newIndex = ((i + positions) % (underlying.size - 1)).toInt

      if (newIndex <= 0) (underlying.size - 1) + newIndex
      else newIndex
    }
  }

  def solvePart1(input: CircularVector[Int]): Long =
    solve(input.map(_.toLong), 1)

  def solvePart2(input: CircularVector[Int]): Long =
    solve(input.map(_.toLong * 811589153), times = 10)

  private def solve(originalVector: CircularVector[Long], times: Int): Long = {
    val finalVector = mix(originalVector, times)
    val indexOfZero = finalVector.underlying.indexOf(0)

    List(1000, 2000, 3000)
      .map(indexOfZero + _)
      .map(finalVector.get)
      .sum
  }

  private def mix(vector: CircularVector[Long], times: Int): CircularVector[Long] =
    (0 until times).foldLeft(vector.zipWithIndex) { (vector, _) => mix(vector) }.map(_._1)

  private def mix(initial: CircularVector[(Long, Int)]): CircularVector[(Long, Int)] =
    initial.underlying.indices
      .foldLeft(initial) { (vector, originalIndex) =>
        val i = vector.underlying.indexWhere(_._2 == originalIndex)

        vector.move(i, vector.get(i)._1)
      }

  private val sample = CircularVector(Input.asList("day20_sample.txt").map(_.toInt).toVector)
  private val input = CircularVector(Input.asList("day20.txt").map(_.toInt).toVector)

  println(solvePart1(sample)) // 3
  println(solvePart1(input)) // 13883

  println(solvePart2(sample)) // 1623178306
  println(solvePart2(input)) // 19185967576920

}
