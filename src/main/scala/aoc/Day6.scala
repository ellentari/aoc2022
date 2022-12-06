package aoc

import scala.annotation.tailrec

object Day6 extends App {

  case class FrequencyCounter[A](counts: Map[A, Int] = Map.empty[A, Int]) {
    def add(a: A): FrequencyCounter[A] =
      copy(counts = counts.updatedWith(a)(count => Some(count.getOrElse(0) + 1)))

    def remove(a: A): FrequencyCounter[A] =
      copy(counts = counts.updatedWith(a)(_.map(_ - 1).filter(_ > 0)))
  }

  case class SlidingWindow[A](
    size: Int,
    over: IndexedSeq[A],
    counter: FrequencyCounter[A] = FrequencyCounter[A]()) {

    def isFull: Boolean = counter.counts.size == size
    def allUnique: Boolean = counter.counts.forall(_._2 == 1)

    def add(index: Int): SlidingWindow[A] =
      if (index < size) copy(counter = counter.add(over(index)))
      else copy(counter = counter.add(over(index)).remove(over(index - size)))
  }

  def solvePart1: String => Option[Int] = solve(uniqueCharCount = 4)

  def solvePart2: String => Option[Int] = solve(uniqueCharCount = 14)

  private def solve(uniqueCharCount: Int)(input: String): Option[Int] = {
    @tailrec
    def loop(i: Int, window: SlidingWindow[Char]): Option[Int] =
      if (window.isFull && window.allUnique) Some(i)
      else if (i >= input.length) None
      else loop(i + 1, window.add(i))

    loop(0, SlidingWindow(uniqueCharCount, input))
  }

  private val input = Input.asString("day6.txt")

  println(solvePart1("mjqjpqmgbljsphdztnvjfqwrcgsmlb")) // 7
  println(solvePart1("bvwbjplbgvbhsrlpgdmjqwftvncz")) // 5
  println(solvePart1("nppdvjthqldpwncqszvftbrmjlhg")) // 6
  println(solvePart1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")) // 10
  println(solvePart1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")) // 11
  println(solvePart1(input)) // 1175

  println(solvePart2("mjqjpqmgbljsphdztnvjfqwrcgsmlb")) // 19
  println(solvePart2("bvwbjplbgvbhsrlpgdmjqwftvncz")) // 23
  println(solvePart2("nppdvjthqldpwncqszvftbrmjlhg")) // 23
  println(solvePart2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")) // 29
  println(solvePart2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")) // 26
  println(solvePart2(input)) // 3217

}
