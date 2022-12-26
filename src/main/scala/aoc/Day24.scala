package aoc

import aoc.algo.BFS
import aoc.util.{Coordinate2D, Direction2D, GridMap}

object Day24 extends App {

  type Direction = Direction2D.HorizontalOrVertical

  sealed trait Cell extends Product with Serializable
  object Cell {
    case object Empty extends Cell
    case object Wall extends Cell
  }

  case class State(position: Coordinate2D, time: Int)

  case class BlizzardMap(map: GridMap[Cell], blizzardMoves: Vector[Set[Coordinate2D]]) {
    val height: Int = map.height
    val width: Int = map.width

    val start: Coordinate2D = map.rowAtY(map.maxY).find(map(_) == Cell.Empty).get
    val end: Coordinate2D = map.rowAtY(map.minY).find(map(_) == Cell.Empty).get

    def blizzardsAtTime(time: Int): Set[Coordinate2D] =
      blizzardMoves(limitTime(time))

    def limitTime(time: Int): Int = time % blizzardMoves.length

    def adjacentEmptyCells(coordinate: Coordinate2D): List[Coordinate2D] =
      map.adjacent4(coordinate).filter(map(_) == Cell.Empty)

  }

  def solvePart1(blizzardMap: BlizzardMap): Int =
    getShortestPathLength(blizzardMap.start, blizzardMap.end, blizzardMap, initialTime = 0)

  def solvePart2(blizzardMap: BlizzardMap): Int = {
    val t1 = getShortestPathLength(blizzardMap.start, blizzardMap.end, blizzardMap, initialTime = 0)
    val t2 = getShortestPathLength(blizzardMap.end, blizzardMap.start, blizzardMap, initialTime = t1)
    val t3 = getShortestPathLength(blizzardMap.start, blizzardMap.end, blizzardMap, initialTime = t1 + t2)

    t1 + t2 + t3
  }

  private def getBlizzardMoves(blizzards: Map[Coordinate2D, Direction], width: Int, height: Int, count: Int): Vector[Set[Coordinate2D]] = {
    val result = (1 to count).view
      .map(time =>
        blizzards.view
          .map { case (coordinate, direction) =>
            moveBlizzard(coordinate.add(-1, 1), direction, time, width - 2, height - 2) // two walls
              .add(1, -1)
          }.toSet
      )
      .toVector

    assert(result.last == blizzards.keySet)

    blizzards.keySet +: result.init
  }

  private def getShortestPathLength(
    start: Coordinate2D,
    end: Coordinate2D,
    blizzardMap: BlizzardMap,
    initialTime: Int
  ): Int =
    BFS.shortestPathLength(State(start, initialTime))(
      state => {
        val movedBlizzards = blizzardMap.blizzardsAtTime(state.time + 1)

        val wait = state.copy(time = blizzardMap.limitTime(state.time + 1))
        val nextMoves =
          blizzardMap.adjacentEmptyCells(state.position)
            .map(State(_, blizzardMap.limitTime(state.time + 1)))

        (wait :: nextMoves)
          .filterNot(next => movedBlizzards.contains(next.position))
      },
      _.position == end,
    )
      .getOrElse(throw new RuntimeException(s"Path from $start to $end doesn't exist!"))

  def moveBlizzard(cc: Coordinate2D, direction: Direction2D, times: Int, width: Int, height: Int): Coordinate2D =
    direction match {
      case Direction2D.Right =>
        val x = (cc.x + times) % width
        Coordinate2D(x, cc.y)
      case Direction2D.Left =>
        val newX = cc.x - (times % width)
        if (newX < 0) Coordinate2D(width + newX, cc.y)
        else Coordinate2D(newX, cc.y)
      case Direction2D.Down =>
        val y = (cc.y - times) % height
        Coordinate2D(cc.x, y)
      case Direction2D.Up =>
        val newY = cc.y + (times % height)
        if (newY > 0) Coordinate2D(cc.x, -height + newY)
        else Coordinate2D(cc.x, newY)
    }

  private def parseBlizzardMap(raw: String): BlizzardMap = {
    val grid = GridMap.parseCharacterGrid(raw)

    createBlizzardMap(grid)
  }

  private def createBlizzardMap(grid: GridMap[Char]) = {
    val blizzards = grid.underlying.collect {
      case (cc, '>') => cc -> Direction2D.Right
      case (cc, 'v') => cc -> Direction2D.Down
      case (cc, '<') => cc -> Direction2D.Left
      case (cc, '^') => cc -> Direction2D.Up
    }

    val gridWithoutBlizzards = grid.map {
      case '#' => Cell.Wall
      case _ => Cell.Empty
    }

    val loopSize = aoc.math.lcm(grid.height - 2, grid.width - 2)
    val blizzardMoves = getBlizzardMoves(blizzards, grid.width, grid.height, loopSize)

    BlizzardMap(gridWithoutBlizzards, blizzardMoves)
  }

  private val sample = parseBlizzardMap(Input.asString("day24_sample.txt"))
  private val input = parseBlizzardMap(Input.asString("day24.txt"))

  println(solvePart1(sample)) // 18
  println(solvePart1(input)) // 283

  println(solvePart2(sample)) // 54
  println(solvePart2(input)) // 883


}
