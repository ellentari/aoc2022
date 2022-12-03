package aoc

object Day3 extends App {

  def solvePart1(input: List[String]): Int =
    input
      .flatMap { s =>
        val (half1, half2) = s.splitAt(s.length / 2)

        half1.toSet & half2.toSet
      }
      .map(getPriority)
      .sum

  def solvePart2(input: List[String]): Int =
    input
      .grouped(3)
      .flatMap(_.map(_.toSet).reduce(_ & _))
      .map(getPriority)
      .sum

  private def getPriority(char: Char) =
    if (char.isLower) char - 'a' + 1
    else char - 'A' + 27

  private val sample = Input.asList("day3_sample.txt")
  private val input = Input.asList("day3.txt")

  println(solvePart1(sample)) // 157
  println(solvePart1(input)) // 8105

  println(solvePart2(sample)) // 70
  println(solvePart2(input)) // 2363

}
