package aoc

import aoc.util.{Coordinate2D, GridMap}

import scala.annotation.tailrec

object Day23 extends App {

  private val Elf = '#'

  case class State(
    round: Int,
    grid: GridMap[Char],
    orderOfLookup: List[Coordinate2D => (List[Coordinate2D], Coordinate2D)]) {

    def elves: Set[Coordinate2D] = grid.filter(_._2 == Elf).map.keySet

    def next(moveElves: List[(Coordinate2D, Coordinate2D)]): State = {
      val nextGrid = grid -- moveElves.map(_._1) ++ moveElves.map(_._2 -> Elf)

      State(
        round = round + 1,
        grid = nextGrid,
        orderOfLookup = orderOfLookup.tail :+ orderOfLookup.head
      )
    }

  }

  def solvePart1(initialGrid: GridMap[Char]): Int = {
    val finalGrid = simulate(initialGrid)((round, _) => round > 10).grid

    (for {
      y <- finalGrid.ys
      x <- finalGrid.xs
      coordinate = Coordinate2D(x, y)
    } yield finalGrid.get(coordinate).forall(_ == '.'))
      .count(identity)
  }

  def solvePart2(initialGrid: GridMap[Char]): Int =
    simulate(initialGrid)((_, moves) => moves.isEmpty).round

  private def simulate(
    initial: GridMap[Char])(
    shouldTerminate: (Int, List[(Coordinate2D, Coordinate2D)]) => Boolean): State = {

    @tailrec
    def loop(state: State): State = {
      val elves = state.elves
      val elfToGoPropositions = state.elves.view
        .filter(_.adjacent8.exists(elves.contains))
        .flatMap { elf =>
          state.orderOfLookup
            .map(_ (elf))
            .find(_._1.forall(!elves.contains(_)))
            .map(_._2)
            .map(elf -> _)
        }
        .toList

      val propositionsCount = elfToGoPropositions
        .groupBy(_._2)
        .view.mapValues(_.size).toMap

      val elvesToGo = elfToGoPropositions
        .filter { case (_, nextPosition) => propositionsCount(nextPosition) == 1 }

      if (shouldTerminate(state.round, elvesToGo)) state
      else loop(state.next(elvesToGo))
    }

    loop(State(1, initial, List(
      (cc: Coordinate2D) => (List(cc.upLeft, cc.up, cc.upRight), cc.up),
      (cc: Coordinate2D) => (List(cc.downLeft, cc.down, cc.downRight), cc.down),
      (cc: Coordinate2D) => (List(cc.upLeft, cc.left, cc.downLeft), cc.left),
      (cc: Coordinate2D) => (List(cc.upRight, cc.right, cc.downRight), cc.right),
    )))
  }

  private val sampleParsed = GridMap.parseCharacterGrid(Input.asString("day23_sample.txt"))
  private val inputParsed = GridMap.parseCharacterGrid(Input.asString("day23.txt"))

  println(solvePart1(sampleParsed)) // 110
  println(solvePart1(inputParsed)) // 3864

  println(solvePart2(sampleParsed)) // 20
  println(solvePart2(inputParsed)) // 946

}
