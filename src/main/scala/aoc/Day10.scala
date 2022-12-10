package aoc

import aoc.Day10.Instruction.{Add, Noop}

object Day10 extends App {

  type Cycle = Int
  type Register = Int

  sealed trait Instruction extends Product with Serializable
  object Instruction {
    case class Add(value: Int) extends Instruction
    case object Noop extends Instruction
  }

  case class Interpreter(instructions: List[Instruction]) {

    def interpret[S](initialState: S)(onCycleStart: (Cycle, Register, S) => S): S = {

      instructions
        .foldLeft((1, 1, initialState)) { case ((cycle, x, state), instruction) =>
          val state1 = onCycleStart(cycle, x, state)

          instruction match {
            case Add(value) =>
              val state2 = onCycleStart(cycle + 1, x, state1)
              (cycle + 2, x + value, state2)
            case Noop =>
              (cycle + 1, x, state1)
          }
        }._3
    }
  }

  class Screen(height: Int, width: Int) {

    private val SpriteLength = 1
    private val DarkPixel = ' '
    private val LightPixel = '#'

    private var spritePosition = 1
    private val screen = Array.fill[Char](height, width)(DarkPixel)

    def updateSpritePosition(value: Int): Unit =
      spritePosition = value

    def draw(position: Int): Unit = {
      val row = position / width
      val col = position % width

      val pixel = if (isVisible(col)) LightPixel else DarkPixel

      screen(row)(col) = pixel
    }

    def toImage(): String = screen.map(_.mkString("")).mkString("\n")

    private def isVisible(pixel: Int) =
      spritePosition - SpriteLength <= pixel &&
        pixel <= spritePosition + SpriteLength
  }

  def solvePart1(instructions: List[Instruction]): Int = {
    val interestingCycles = List(20, 60, 100, 140, 180, 220)

    Interpreter(instructions).interpret(0) { case (cycle, x, acc) =>
      if (interestingCycles.contains(cycle)) acc + (cycle * x)
      else acc
    }
  }

  def solvePart22(instructions: List[Instruction]): String = {
    val screen = new Screen(height = 6, width = 40)

    Interpreter(instructions).interpret(()) { case (cycle, x, _) =>
      screen.updateSpritePosition(x)
      screen.draw(cycle - 1)
    }

    screen.toImage()
  }

  private def parseInstruction(raw: String): Instruction = raw match {
    case s"addx $v" => Add(v.toInt)
    case "noop" => Noop
  }

  private val sample = Input.asList("day10_sample.txt").map(parseInstruction)
  private val input = Input.asList("day10.txt").map(parseInstruction)

  println(solvePart1(sample)) // 13140
  println(solvePart1(input)) // 17180

  println(solvePart22(sample))
  println()
  println(solvePart22(input)) // REHPRLUB

}
