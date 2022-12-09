package aoc

import aoc.util.Direction2D._
import aoc.util.{Coordinate2D, Delta2D, Direction2D}

object Day9 extends App {

  case class Motion(direction: Direction2D, steps: Int)

  case class Rope(knotPositions: Vector[Coordinate2D]) {
    def moveHead(direction: Direction2D): Rope = {
      val headMoved = knotPositions.updated(0, direction.delta.applyTo(knotPositions(0)))
      val tailsMoved = (1 until knotPositions.length)
        .foldLeft(headMoved) { (positions, i) =>
          val delta = deltaForFollower(positions(i - 1), positions(i))

          positions.updated(i, delta.applyTo(positions(i)))
        }

      Rope(tailsMoved)
    }

    private def deltaForFollower(leader: Coordinate2D, follower: Coordinate2D) = {

      def closeEnough(x1: Int, x2: Int) = (x1 - x2).abs <= 1
      def delta(x1: Int, x2: Int) = if (x1 == x2) 0 else if (x1 > x2) 1 else -1

      if (closeEnough(leader.x, follower.x) && closeEnough(leader.y, follower.y)) Delta2D.Zero
      else Delta2D(delta(leader.x, follower.x), delta(leader.y, follower.y))
    }
  }

  case class State(rope: Rope, trackedPositions: Set[Coordinate2D] = Set.empty) {
    def update(newRope: Rope): State =
      copy(
        rope = newRope,
        trackedPositions = trackedPositions + newRope.knotPositions.last
      )
  }

  def solvePart11: List[Motion] => Int = solve(2)

  def solvePart22: List[Motion] => Int = solve(10)

  private def solve(n: Int)(input: List[Motion]): Int = {

    def nextState(state: State, direction: Direction2D): State =
      state.update(state.rope.moveHead(direction))

    val initialState = State(Rope(Vector.fill(n)(Coordinate2D.Zero)), Set(Coordinate2D.Zero))
    val finalState = input
      .flatMap(motion => (0 until motion.steps).map(_ => motion.direction))
      .foldLeft(initialState)(nextState)

    finalState.trackedPositions.size
  }

  private def parseMotion(raw: String) = raw match {
    case s"R $steps" => Motion(Right, steps.toInt)
    case s"L $steps" => Motion(Left, steps.toInt)
    case s"U $steps" => Motion(Up, steps.toInt)
    case s"D $steps" => Motion(Down, steps.toInt)
  }

  private val sample = Input.asList("day9_sample.txt").map(parseMotion)
  private val sample2 = Input.asList("day9_sample2.txt").map(parseMotion)
  private val input = Input.asList("day9.txt").map(parseMotion)

  println(solvePart11(sample)) // 13
  println(solvePart11(input)) // 6339

  println(solvePart22(sample)) // 1
  println(solvePart22(sample2)) // 36
  println(solvePart22(input)) // 2541

}
