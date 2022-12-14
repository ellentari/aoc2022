package aoc

import aoc.util.{Coordinate2D, Vector2D}

import scala.annotation.tailrec

object Day14 extends App {

  type Wall = Vector2D

  sealed trait Cell extends Product with Serializable
  object Cell {
    case object Rock extends Cell
    case object Sand extends Cell
  }

  case class Grid(map: Map[Coordinate2D, Cell], floor: Option[Int] = None) {
    def get(coordinate: Coordinate2D): Option[Cell] =
      if (floor.contains(coordinate.y)) Some(Cell.Rock)
      else map.get(coordinate)

    def add(coordinate: Coordinate2D, cell: Cell): Grid =
      copy(map = map + (coordinate -> cell))

    def lowestY: Option[Int] =
      floor.orElse(map.keySet.minByOption(_.y).map(_.y))

    def isEmpty(coordinate: Coordinate2D): Boolean = get(coordinate).isEmpty
  }

  object Grid {
    def fromRocks(walls: List[Wall], floor: Option[Int] = None): Grid =
      Grid(walls.flatMap(_.coordinates)
        .map(_ -> Cell.Rock)
        .toMap, floor)
  }

  private val Source = Coordinate2D(500, 0)

  def solvePart1(walls: List[Vector2D]): Int = simulateSandFall(Grid.fromRocks(walls))

  def solvePart2(input: List[Vector2D]): Int = {
    val floor = input.map(_.minY).minOption.map(_ - 2)

    simulateSandFall(Grid.fromRocks(input, floor))
  }

  private def simulateSandFall(initial: Grid) = {

    @tailrec
    def fallDown(coordinate: Coordinate2D, grid: Grid): Option[Coordinate2D] = {
      if (grid.lowestY.forall(coordinate.y <= _)) None
      else {
        val toFall = List(coordinate.down, coordinate.downLeft, coordinate.downRight)
          .find(grid.isEmpty)

        toFall match {
          case Some(next) => fallDown(next, grid)
          case None => Some(coordinate)
        }
      }
    }

    @tailrec
    def loop(grid: Grid, count: Int): Int = {
      fallDown(Source, grid) match {
        case None => count
        case Some(cc) if cc == Source => count + 1
        case Some(cc) => loop(grid.add(cc, Cell.Sand), count + 1)
      }
    }

    loop(initial, 0)
  }

  private def parsWalls(raw: String): List[Wall] =
    raw
      .split(" -> ")
      .map { s =>
        val parts = s.split(",")
        Coordinate2D(parts(0).trim.toInt, -parts(1).trim.toInt)
      }
      .sliding(2)
      .map { coords =>
        Vector2D.from(coords(0), coords(1))
      }
      .toList

  private val sample =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin.split("\n").toList

  private val sampleParsed = sample.flatMap(parsWalls)

  private val input = Input.asList("day14.txt")
  private val inputParsed = input.flatMap(parsWalls)

  println(solvePart1(sampleParsed)) // 24
  println(solvePart1(inputParsed)) // 1133

  println(solvePart2(sampleParsed)) // 93
  println(solvePart2(inputParsed)) // 27566

}
