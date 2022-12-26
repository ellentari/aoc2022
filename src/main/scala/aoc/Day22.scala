package aoc

import aoc.util.{Coordinate2D, Direction2D, GridMap}
import aoc.util.Direction2D._
import cats.data.NonEmptyList

import scala.annotation.tailrec

object Day22 extends App {

  type Direction = Direction2D.HorizontalOrVertical

  sealed trait PathInstruction extends Product with Serializable
  object PathInstruction {
    case class Go(steps: Int) extends PathInstruction
    case object TurnRight extends PathInstruction
    case object TurnLeft extends PathInstruction
  }

  import PathInstruction._

  sealed trait CubeFace extends Product with Serializable
  object CubeFace {
    case object A extends CubeFace
    case object B extends CubeFace
    case object C extends CubeFace
    case object D extends CubeFace
    case object E extends CubeFace
    case object F extends CubeFace
  }

  import CubeFace._

  case class CubeSide(face: CubeFace, topLeft: Coordinate2D, orientation: Direction, size: Int) {

    val topRight: Coordinate2D = topLeft.copy(x = topLeft.x + size - 1)
    val bottomLeft: Coordinate2D = topLeft.copy(y = topLeft.y - size + 1)
    val bottomRight: Coordinate2D = Coordinate2D(x = topRight.x, y = bottomLeft.y)

    val rightTurnsFromUp: Int = orientation match {
      case Up => 0
      case Right => 1
      case Down => 2
      case Left => 3
    }

    def contains(coordinate: Coordinate2D): Boolean =
      topLeft.x <= coordinate.x &&
        coordinate.x <= topRight.x &&
        topLeft.y >= coordinate.y &&
        bottomLeft.y <= coordinate.y

    def right(face: CubeFace, orientation: Direction): CubeSide =
      copy(topLeft = topLeft.addX(size), face = face, orientation = orientation)
    def bottom(face: CubeFace, orientation: Direction): CubeSide =
      copy(topLeft = topLeft.addY(-size), face = face, orientation = orientation)
    def left(face: CubeFace, orientation: Direction): CubeSide =
      copy(topLeft = topLeft.addX(-size), face = face, orientation = orientation)
    def up(face: CubeFace, orientation: Direction): CubeSide =
      copy(topLeft = topLeft.addY(size), face = face, orientation = orientation)

    def rotateClockwise(cc: Coordinate2D): Coordinate2D = {
      val dx = cc.x - topLeft.x
      val dy = (cc.y - topLeft.y).abs

      Coordinate2D(topLeft.x + size - 1 - dy, topLeft.y - dx)
    }

    def rotateClockwiseTimes(cc: Coordinate2D, times: Int): Coordinate2D =
      (0 until times % 4).foldLeft(cc)((cc1, _) => rotateClockwise(cc1))

    def rotateCounterclockwiseTimes(cc: Coordinate2D, times: Int): Coordinate2D =
      rotateClockwiseTimes(cc, 4 - (times % 4))

    def topRowFromLeft(dx: Int): Coordinate2D = topLeft.addX(dx)
    def topRowFromRight(dx: Int): Coordinate2D = topRight.addX(-dx)

    def bottomRowFromLeft(dx: Int): Coordinate2D = bottomLeft.addX(dx)
    def bottomRowFromRight(dx: Int): Coordinate2D = bottomRight.addX(-dx)

    def leftColFromTop(dy: Int): Coordinate2D = topLeft.addY(-dy)
    def leftColFromBottom(dy: Int): Coordinate2D = bottomLeft.addY(dy)

    def rightColFromTop(dy: Int): Coordinate2D = topRight.addY(-dy)
    def rightColFromBottom(dy: Int): Coordinate2D = bottomRight.addY(dy)

  }

  sealed trait Cell extends Product with Serializable
  object Cell {
    case object Empty extends Cell
    case object Wall extends Cell
  }

  case class Cube(grid: GridMap[Cell], sides: NonEmptyList[CubeSide]) {

    def side(face: CubeFace): CubeSide =
      sides.find(_.face == face)
        .getOrElse(throw new RuntimeException(s"No cube side with face $face"))

    def rightTurnsFromUp(face: CubeFace): Int = side(face).rightTurnsFromUp

    def getCubeSide(coordinate: Coordinate2D): CubeSide =
      sides.find(_.contains(coordinate))
        .getOrElse(throw new RuntimeException(s"No cube side contains coordinate $coordinate"))

    def getEntryCoordinate(
      from: Coordinate2D,
      fromDirection: Direction,
      to: CubeFace,
      toDirection: Direction): Coordinate2D = {
      val cubeSide = side(to)
      val dy = (from.y % sides.head.size).abs
      val dx = from.x % sides.head.size

      (fromDirection, toDirection) match {
        case (Right, Down) => cubeSide.topRowFromRight(dy)
        case (Right, Up) => cubeSide.bottomRowFromLeft(dy)
        case (Right, Right) => cubeSide.leftColFromTop(dy)
        case (Right, Left) => cubeSide.rightColFromBottom(dy)

        case (Left, Down) => cubeSide.topRowFromLeft(dy)
        case (Left, Up) => cubeSide.bottomRowFromRight(dy)
        case (Left, Right) => cubeSide.leftColFromBottom(dy)
        case (Left, Left) => cubeSide.rightColFromTop(dy)

        case (Down, Down) => cubeSide.topRowFromLeft(dx)
        case (Down, Up) => cubeSide.bottomRowFromRight(dx)
        case (Down, Right) => cubeSide.leftColFromBottom(dx)
        case (Down, Left) => cubeSide.rightColFromTop(dx)

        case (Up, Down) => cubeSide.topRowFromRight(dx)
        case (Up, Up) => cubeSide.bottomRowFromLeft(dx)
        case (Up, Right) => cubeSide.leftColFromTop(dx)
        case (Up, Left) => cubeSide.rightColFromBottom(dx)
      }
    }
  }

  case class State(position: Coordinate2D, direction: Direction)

  private lazy val Transitions: Map[(CubeFace, Direction), (CubeFace, Direction)] = Map(
    (A, Up) -> (B, Down),
    (A, Right) -> (F, Left),
    (A, Down) -> (D, Down),
    (A, Left) -> (C, Down),

    (B, Up) -> (A, Down),
    (B, Right) -> (C, Right),
    (B, Down) -> (E, Up),
    (B, Left) -> (F, Up),

    (C, Up) -> (A, Right),
    (C, Right) -> (D, Right),
    (C, Down) -> (E, Right),
    (C, Left) -> (B, Left),

    (D, Up) -> (A, Up),
    (D, Right) -> (F, Down),
    (D, Down) -> (E, Down),
    (D, Left) -> (C, Left),

    (E, Up) -> (D, Up),
    (E, Right) -> (F, Right),
    (E, Down) -> (B, Up),
    (E, Left) -> (C, Up),

    (F, Up) -> (D, Left),
    (F, Right) -> (A, Left),
    (F, Down) -> (B, Right),
    (F, Left) -> (E, Left)
  )

  def solvePart1(cube: Cube, instructions: List[PathInstruction]): Int =
    solve(wrapPart1(cube))(cube, instructions)

  def solvePart2(cube: Cube, instructions: List[PathInstruction]): Int =
    solve(wrapPart2(cube))(cube, instructions)

  private def wrapPart1(cube: Cube)(state: State): State = {
    val direction = state.direction.opposite
    val nextPosition = (1 until (cube.grid.height max cube.grid.width)).view
      .map(direction.delta.multiply)
      .map(state.position.add)
      .takeWhile(cube.grid.contains)
      .last

    state.copy(position = nextPosition)
  }

  private def wrapPart2(cube: Cube)(state: State): State = {
    val currentCubeSide = cube.getCubeSide(state.position)

    val directionAsOfCurrentCubeFace =
      state.direction.rotateCounterclockwise(currentCubeSide.rightTurnsFromUp)

    val (nextCubeFace, nextDirectionAsOfNextCubeFace) =
      Transitions(currentCubeSide.face -> directionAsOfCurrentCubeFace)

    val nextCubeSide = cube.side(nextCubeFace)
    val nextDirection =
      nextDirectionAsOfNextCubeFace.rotateClockwise(nextCubeSide.rightTurnsFromUp)

    val nextPosition = cube.getEntryCoordinate(
      currentCubeSide.rotateCounterclockwiseTimes(state.position, nextCubeSide.rightTurnsFromUp),
      state.direction.rotateCounterclockwise(nextCubeSide.rightTurnsFromUp),
      nextCubeFace,
      nextDirection)

    State(nextPosition, nextDirection)
  }

  private def solve(wrap: State => State)(cube: Cube, instructions: List[PathInstruction]) = {
    val start = cube.grid.rowAtY(cube.grid.maxY).find(cube.grid.contains).get
    val initialState = State(start, Right)
    val finalState = instructions.foldLeft(initialState)(move(cube, wrap))

    val facing = finalState.direction match {
      case Right => 0
      case Down => 1
      case Left => 2
      case Up => 3
    }

    (finalState.position.y.abs + 1) * 1000 + (finalState.position.x + 1) * 4 + facing
  }

  private def move(cube: Cube, wrap: State => State)(state: State, instruction: PathInstruction): State = {
    @tailrec
    def go(state: State, steps: Int): State =
      if (steps == 0) state
      else {
        val nextPosition = state.position.add(state.direction.delta)
        val wrapped =
          if (!cube.grid.contains(nextPosition)) wrap(state)
          else state.copy(position = nextPosition)

        assert(cube.grid.contains(wrapped.position))

        if (cube.grid.get(wrapped.position).contains(Cell.Empty)) go(wrapped, steps - 1)
        else state
      }

    instruction match {
      case Go(steps) => go(state, steps)
      case TurnRight => state.copy(direction = state.direction.turnRight)
      case TurnLeft => state.copy(direction = state.direction.turnLeft)
    }
  }

  private def parseInput(raw: String): (GridMap[Cell], List[PathInstruction]) = {
    val parts = raw.split("\n\n")
    val gridMap = GridMap.parseCharacterGrid(parts(0))
      .filter(_._2 != ' ')
      .map {
        case '.' => Cell.Empty
        case '#' => Cell.Wall
      }

    val instructions = parsePathInstructions(parts(1))

    (gridMap, instructions)
  }

  private def parsePathInstructions(raw: String): List[PathInstruction] = {
    @tailrec
    def loop(i: Int, acc: List[PathInstruction]): List[PathInstruction] = {
      if (i >= raw.length) acc.reverse
      else if (raw(i).isLetter) {
        val inst = raw(i) match {
          case 'R' => TurnRight
          case 'L' => TurnLeft
        }

        loop(i + 1, inst :: acc)
      } else {
        var j = i + 1
        while (j < raw.length && raw(j).isDigit)
          j += 1

        loop(j, Go(raw.substring(i, j).toInt) :: acc)
      }
    }

    loop(0, Nil)
  }

  // Sample:
  //          +----+
  //          | A↑ |
  //+----+----+----+
  //| B↑ | C↑ | D↑ |
  //+----+----+----+----+
  //          | E↑ | F↑ |
  //          +----+----+
  private val (sampleGrid, sampleInstructions) = parseInput(Input.asString("day22_sample.txt"))
  private val sampleCubeSideSize = 4
  private val sampleCubeSideA = CubeSide(A, Coordinate2D(sampleCubeSideSize * 2, 0), Up, sampleCubeSideSize)
  private val sampleCubeSideD = sampleCubeSideA.bottom(D, Up)
  private val sampleCubeSideC = sampleCubeSideD.left(C, Up)
  private val sampleCubeSideB = sampleCubeSideC.left(B, Up)
  private val sampleCubeSideE = sampleCubeSideD.bottom(E, Up)
  private val sampleCubeSideF = sampleCubeSideE.right(F, Up)
  private val sampleCubeSides = NonEmptyList.of(
    sampleCubeSideA,
    sampleCubeSideB,
    sampleCubeSideC,
    sampleCubeSideD,
    sampleCubeSideE,
    sampleCubeSideF
  )
  private val sampleCube = Cube(sampleGrid, sampleCubeSides)

  // Input:
  //        +----+----+
  //        | A↑ | F↓ |
  //        +----+----+
  //        | D↑ |
  //   +----+----+
  //   | C← | E↑ |
  //   +----+----+
  //   | B← |
  //   +----+
  private val (inputGrid, inputInstructions) = parseInput(Input.asString("day22.txt"))
  private val inputCubeSideSize = 50
  private val inputCubeSideA = CubeSide(A, Coordinate2D(inputCubeSideSize, 0), Up, inputCubeSideSize)
  private val inputCubeSideF = inputCubeSideA.right(F, Down)
  private val inputCubeSideD = inputCubeSideA.bottom(D, Up)
  private val inputCubeSideE = inputCubeSideD.bottom(E, Up)
  private val inputCubeSideC = inputCubeSideE.left(C, Left)
  private val inputCubeSideB = inputCubeSideC.bottom(B, Left)
  private val inputCubeSides = NonEmptyList.of(
    inputCubeSideA,
    inputCubeSideB,
    inputCubeSideC,
    inputCubeSideD,
    inputCubeSideE,
    inputCubeSideF
  )
  private val inputCube = Cube(inputGrid, inputCubeSides)

  println(solvePart1(sampleCube, sampleInstructions)) // 6032
  println(solvePart1(inputCube, inputInstructions)) // 136054

  println(solvePart2(sampleCube, sampleInstructions)) // 5031
  println(solvePart2(inputCube, inputInstructions)) // 122153

}
