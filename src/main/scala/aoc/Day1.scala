package aoc

object Day1 extends App {

  def solvePart1(input: List[List[Int]]): Option[Int] = input.map(_.sum).maxOption

  def solvePart2(input: List[List[Int]]): Int = input.map(_.sum).sortBy(-_).take(3).sum

  private def parseInput(raw: String) =
    raw
      .split("\n\n")
      .map(_.split("\n").map(_.toInt).toList)
      .toList

  private val sample = parseInput(Input.asString("day1_sample.txt"))
  private val input = parseInput(Input.asString("day1.txt"))

  println(solvePart1(sample)) // 24000
  println(solvePart1(input)) // 64929

  println(solvePart2(sample)) // 45000
  println(solvePart2(input)) // 193697

}
