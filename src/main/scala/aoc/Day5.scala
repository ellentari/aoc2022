package aoc

object Day5 extends App {

  type Crate = Char

  case class Stack[A](value: List[A]) {
    def removeTopN(n: Int): Stack[A] = copy(value = value.drop(n))
    def prepend(crates: List[A]): Stack[A] = copy(value = crates ::: value)
  }

  case class Move(count: Int, fromStack: Int, toStack: Int)

  case class State(crateStacks: Vector[Stack[Crate]]) {

    def moveOneByOne: Move => State = makeMove(_.reverse)
    def moveAllAtOnce: Move => State = makeMove(identity)

    private def makeMove(transform: List[Crate] => List[Crate])(move: Move) = {
      val toMove = crateStacks(move.fromStack).value.take(move.count)

      State(
        crateStacks
          .updated(move.fromStack, crateStacks(move.fromStack).removeTopN(move.count))
          .updated(move.toStack, crateStacks(move.toStack).prepend(transform(toMove)))
      )
    }
  }

  object Parser {
    def parseInput(input: String): (List[Stack[Crate]], List[Move]) = {
      val parts = input.split("\n\n")

      val stacksPart = parts(0)
      val movesPart = parts(1)

      val crates = parseStacks(stacksPart)
      val moves = movesPart.split("\n").map(parseMove).toList

      (crates, moves)
    }

    private def parseStacks(input: String): List[Stack[Crate]] = {
      val lines = input.split("\n").toList.reverse
      val stackIds = lines.head
      val stacks = lines.tail

      def parseStack(i: Int): Stack[Crate] = {
        val stack = stacks
          .flatMap { line =>
            Option.when(i < line.length)(line(i)).filter(_.isLetter)
          }
          .reverse

        Stack(stack)
      }

      stackIds.indices
        .filter(stackIds(_).isDigit)
        .map(parseStack)
        .toList
    }

    private def parseMove(move: String): Move = move match {
      case s"move $count from $from to $to" =>
        Move(count = count.toInt, fromStack = from.toInt - 1, toStack = to.toInt - 1)
    }

  }

  def solvePart1: (List[Stack[Crate]], List[Move]) => String = solve(_.moveOneByOne(_))

  def solvePart2: (List[Stack[Crate]], List[Move]) => String = solve(_.moveAllAtOnce(_))

  private def solve(move: (State, Move) => State)(crateStacks: List[Stack[Crate]], moves: List[Move]) =
    moves.foldLeft(State(crateStacks.toVector))(move)
      .crateStacks.flatMap(_.value.headOption)
      .mkString("")

  private val sample = Parser.parseInput(Input.asString("day5_sample.txt"))
  private val input = Parser.parseInput(Input.asString("day5.txt"))

  println(solvePart1.tupled(sample)) // CMZ
  println(solvePart1.tupled(input)) // BZLVHBWQF

  println(solvePart2.tupled(sample)) // MCD
  println(solvePart2.tupled(input)) // TDGJQTZSL

}
