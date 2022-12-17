package aoc

import aoc.util.{Coordinate2D, Delta2D, Direction2D, GridMap}

import scala.annotation.tailrec

object Day17 extends App {

  private val GridWidth = 7
  private val XOffset = 2
  private val YOffset = 4
  private val Rock = '#'

  sealed trait Figure extends Product with Serializable {
    def bottomLeft: Coordinate2D
    def move(delta: Delta2D): Figure
    def coordinates: Set[Coordinate2D]
  }

  object Figure {

    case class HorizontalLine(bottomLeft: Coordinate2D) extends Figure {

      def coordinates: Set[Coordinate2D] =
        (0 until 4).map(bottomLeft.addX).toSet

      def move(delta: Delta2D): Figure =
        HorizontalLine(delta.applyTo(bottomLeft))
    }

    case class VerticalLine(bottomLeft: Coordinate2D) extends Figure {

      def coordinates: Set[Coordinate2D] =
        (0 until 4).map(bottomLeft.addY).toSet

      def move(delta: Delta2D): Figure =
        VerticalLine(delta.applyTo(bottomLeft))
    }

    case class Square(bottomLeft: Coordinate2D) extends Figure {

      def coordinates: Set[Coordinate2D] =
        (for {
          dx <- 0 until 2
          dy <- 0 until 2
        } yield bottomLeft.add(dx, dy)).toSet

      def move(delta: Delta2D): Figure =
        Square(delta.applyTo(bottomLeft))
    }

    case class Cross(bottomLeft: Coordinate2D) extends Figure {
      def coordinates: Set[Coordinate2D] = Set(
        bottomLeft.right,
        bottomLeft.up,
        bottomLeft.up.right,
        bottomLeft.up.right.right,
        bottomLeft.up.up.right,
      )

      def move(delta: Delta2D): Figure =
        Cross(delta.applyTo(bottomLeft))
    }

    case class Angle(bottomLeft: Coordinate2D) extends Figure {
      def coordinates: Set[Coordinate2D] = Set(
        bottomLeft,
        bottomLeft.right,
        bottomLeft.right.right,
        bottomLeft.up.right.right,
        bottomLeft.up.up.right.right,
      )

      def move(delta: Delta2D): Figure =
        Angle(delta.applyTo(bottomLeft))
    }

  }

  case class State(
    moveI: Int,
    figureI: Int,
    grid: GridMap[Char],
    heightDiffs: Vector[Int]) {
    def totalHeight: Int = heightDiffs.sum
    def calculateHeight(start: Int, end: Int): Int =
      (start until end).view.map(heightDiffs(_)).sum
  }

  def solvePart1(input: Vector[Direction2D.Horizontal]): Int =
    simulate(input, rounds = 2022).totalHeight

  def solvePart2(input: Vector[Direction2D.Horizontal]): Long = {
    val totalRounds = 1000000000000L
    // empirically derived number that is enough to detect a cycle
    val numberOfActualSimulations = 2100

    // simulate some number of rounds
    val finalState = simulate(input, numberOfActualSimulations)

    // detect a cycle in height diffs
    val (cycleStart, cycleLength) = detectCycle(finalState.heightDiffs)
      .getOrElse(throw new RuntimeException("No cycle detected!"))

    // calculate total height from cycle
    val lengthAfterCycleStart = totalRounds - cycleStart
    val cyclesCount = lengthAfterCycleStart / cycleLength
    val nAfterFullCyclesEnd = (lengthAfterCycleStart % cycleLength).toInt

    val heightBeforeCycles = finalState.calculateHeight(0, cycleStart)
    val cycleHeight = finalState.calculateHeight(cycleStart, cycleStart + cycleLength)
    val remainingHeight = finalState.calculateHeight(cycleStart, cycleStart + nAfterFullCyclesEnd)

    // height = height before cycles start +
    //          height of cycle * number of cycles +
    //          height after last full cycle ends
    heightBeforeCycles.toLong +
      cyclesCount * cycleHeight +
      remainingHeight
  }

  private def simulate(input: Vector[Direction2D.Horizontal], rounds: Int): State = {
    val figuresInOrder = Vector(
      Figure.HorizontalLine.apply _,
      Figure.Cross.apply _,
      Figure.Angle.apply _,
      Figure.VerticalLine.apply _,
      Figure.Square.apply _
    )

    (0 until rounds)
      .foldLeft(State(0, 0, GridMap.empty[Char], Vector.empty[Int])) {
        case (state, _) =>
          val (lastInstruction, nextGrid) = simulateFall(
            state.moveI, input, figuresInOrder(state.figureI % figuresInOrder.length), state.grid)
          val heightDiff = nextGrid.maxY - state.grid.maxYOption.getOrElse(-1)

          State(lastInstruction + 1, state.figureI + 1, nextGrid, state.heightDiffs :+ heightDiff)
      }
  }

  private def simulateFall(
    firstInstruction: Int,
    moves: Vector[Direction2D.Horizontal],
    initialFigure: Coordinate2D => Figure,
    grid: GridMap[Char]): (Int, GridMap[Char]) = {

    def isSafeMove(figure: Figure) = {
      val coordinates = figure.coordinates

      coordinates.map(_.x).minOption.forall(_ >= 0) &&
        coordinates.map(_.y).minOption.forall(_ >= 0) &&
        coordinates.map(_.x).maxOption.forall(_ < GridWidth) &&
        coordinates.intersect(grid.map.keySet).isEmpty
    }

    @tailrec
    def loop(i: Int, figure: Figure): (Int, GridMap[Char]) = {
      val shifted = figure.move(moves(i % moves.length).delta)
      val afterShift =
        if (isSafeMove(shifted)) shifted
        else figure

      val fell = afterShift.move(Direction2D.Down.delta)

      if (isSafeMove(fell)) loop(i + 1, fell)
      else (i, grid ++ afterShift.coordinates.map(_ -> Rock))
    }

    val bottomLeft = Coordinate2D(
      XOffset,
      grid.maxYOption.getOrElse(-1) + YOffset)

    loop(firstInstruction, initialFigure(bottomLeft))
  }

  private def detectCycle(vector: Vector[Int]): Option[(Int, Int)] = {
    val minCycleLength = 20
    (for {
      start <- (0 to vector.length - minCycleLength).view
      slice = vector.slice(start, start + minCycleLength)
      end = vector.indexOfSlice(slice, start + 1) if end >= start + slice.length
    } yield (start, end - start)).headOption
  }

  private def parseDirections(s: String): Vector[Direction2D.Horizontal] =
    s.map {
      case '>' => Direction2D.Right
      case '<' => Direction2D.Left
    }.toVector

  private val sample = parseDirections(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
  private val input = parseDirections(Input.asString("day17.txt"))

  println(solvePart1(sample)) // 3068
  println(solvePart1(input)) // 3141

  println(solvePart2(sample)) // 1514285714288
  println(solvePart2(input)) // 1561739130391

}
