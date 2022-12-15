package aoc

import aoc.util.Interval

object Day4 extends App {

  def solvePart1(input: List[(Interval, Interval)]): Int =
    input.count { case (interval1, interval2) =>
      interval1.contains(interval2) || interval2.contains(interval1)
    }

  def solvePart2(input: List[(Interval, Interval)]): Int =
    input.count { case (interval1, interval2) =>
      interval1.overlaps(interval2)
    }

  private def parseIntervals(input: String): (Interval, Interval) =
    input match {
      case s"$s1-$e1,$s2-$e2" =>
        (Interval(s1.toInt, e1.toInt), Interval(s2.toInt, e2.toInt))
    }

  private val sample = Input.asList("day4_sample.txt").map(parseIntervals)
  private val input = Input.asList("day4.txt").map(parseIntervals)

  println(solvePart1(sample)) // 2
  println(solvePart1(input)) // 475

  println(solvePart2(sample)) // 4
  println(solvePart2(input)) // 825

}
