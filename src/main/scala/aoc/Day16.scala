package aoc

import scala.collection.mutable

object Day16 extends App {

  private val StartValve = "AA"
  private val MinutesPart1 = 30
  private val MinutesPart2 = 26

  type ValveName = String

  case class Valve(name: ValveName, flowRate: Int, connectedTo: List[ValveName])

  sealed trait Valves {
    def all: Set[ValveName]
    def flowRate(valve: ValveName): Int
    def adjacent(valve: ValveName): Map[ValveName, Int]
    def limitTo(to: Set[ValveName]): Valves =
      Valves.Limited(to, this)
  }

  object Valves {
    case class Unlimited(
      all: Set[ValveName],
      flowRates: Map[ValveName, Int],
      distances: Map[ValveName, Map[ValveName, Int]]) extends Valves {

      def flowRate(valve: ValveName): Int = flowRates.getOrElse(valve, 0)
      def adjacent(valve: ValveName): Map[ValveName, Int] = distances.getOrElse(valve, Map.empty)
    }

    case class Limited(limitedTo: Set[ValveName], underlying: Valves) extends Valves {
      def all: Set[ValveName] = limitedTo
      def flowRate(valve: ValveName): Int = underlying.flowRate(valve)
      def adjacent(valve: ValveName): Map[ValveName, Int] =
        underlying.adjacent(valve).view.filterKeys(limitedTo.contains).toMap
    }
  }

  def solvePart1(input: Valves): Int = findMaxFlowRate(input, MinutesPart1)

  def solvePart2(input: Valves): Int =
    generateDisjointSubsets(input.all)
      .map { case (mineToVisit, elephantToVisit) =>
        val myResult = findMaxFlowRate(input.limitTo(mineToVisit), MinutesPart2)
        val elephantResult = findMaxFlowRate(input.limitTo(elephantToVisit), MinutesPart2)

        myResult + elephantResult
      }
      .max

  private def generateDisjointSubsets(all: Set[ValveName]) =
    (1 to all.size / 2).iterator
      .flatMap(all.subsets)
      .map { first => (first, all -- first) }

  private def findMaxFlowRate(valves: Valves, totalMinutes: Int): Int = {
    val memo = mutable.HashMap.empty[(ValveName, Int, Set[ValveName]), Int]

    def loop(current: ValveName, remainingMinutes: Int, openValves: Set[ValveName]): Int =
      if (remainingMinutes <= 0) 0
      else if (memo.contains((current, remainingMinutes, openValves)))
        memo((current, remainingMinutes, openValves))
      else {
        val result = valves.adjacent(current)
          .filter { case (next, distance) =>
            !openValves.contains(next) &&
              remainingMinutes > distance
          }
          .map { case (next, distance) =>
            val nextRemainingMinutes = remainingMinutes - distance - 1
            val value = valves.flowRate(next) * nextRemainingMinutes

            value + loop(next, nextRemainingMinutes, openValves + next)
          }
          .maxOption
          .getOrElse(0)

        memo.update((current, remainingMinutes, openValves), result)

        result
      }

    loop(StartValve, totalMinutes, Set.empty)
  }

  private def preprocessInput(valves: List[Valve]): Valves = {
    val nonZeroValves = valves.filter(_.flowRate != 0).map(_.name).toSet
    val flowRates = valves.map(v => v.name -> v.flowRate).toMap
    val distances = buildDistanceMap(valves, nonZeroValves + StartValve).view
      .mapValues(_.view.filterKeys(_ != StartValve).toMap)
      .toMap

    Valves.Unlimited(nonZeroValves, flowRates, distances)
  }

  private def buildDistanceMap(
    allValves: List[Valve],
    interestingValves: Set[ValveName]
  ): Map[ValveName, Map[ValveName, Int]] = {
    val adjList = allValves.map(v => v.name -> v.connectedTo).toMap
    val allNodes: Vector[ValveName] = allValves.map(_.name).toVector

    val distance = buildDistanceMatrix(allNodes, adjList)

    val allMap: Map[ValveName, Map[ValveName, Int]] =
      (for {
        i <- distance.indices
        j <- distance.indices if i != j
      } yield (allNodes(i), allNodes(j), distance(i)(j)))
        .groupMap(_._1)(t => t._2 -> t._3)
        .view
        .mapValues(_.toMap)
        .toMap

    allMap
      .view
      .filterKeys(interestingValves.contains)
      .mapValues(_.view.filterKeys(interestingValves.contains).toMap)
      .toMap
  }

  private def buildDistanceMatrix(
    nodes: Vector[ValveName],
    adjList: Map[ValveName, List[ValveName]]): Vector[Vector[Int]] = {
    val distance = Array.fill[Int](nodes.size, nodes.size)(Int.MaxValue)
    val nodesToIndex = nodes.zipWithIndex.toMap

    for (i <- nodes.indices)
      distance(i)(i) = 0

    for {
      from <- nodes
      to <- adjList.getOrElse(from, Nil)
    }
      distance(nodesToIndex(from))(nodesToIndex(to)) = 1

    for {
      intermediate <- nodes.indices
      from <- nodes.indices
      to <- nodes.indices
    } {
      val distThroughIntermediate = distance(from)(intermediate).toLong + distance(intermediate)(to)

      if (distThroughIntermediate < distance(from)(to))
        distance(from)(to) = distThroughIntermediate.toInt
    }

    distance.map(_.toVector).toVector
  }

  private def parseValve(raw: String) =
    raw match {
      case s"Valve $id has flow rate=$flowRate; tunnel leads to valve $single" =>
        Valve(id, flowRate.toInt, List(single))
      case s"Valve $id has flow rate=$flowRate; tunnels lead to valves $list" =>
        Valve(id, flowRate.toInt, list.split(", ").toList)
    }

  private val sample = preprocessInput(Input.asList("day16_sample.txt").map(parseValve))
  private val input = preprocessInput(Input.asList("day16.txt").map(parseValve))

  solvePart1(sample) // 1651
  solvePart1(input) // 1751

  solvePart2(sample) // 1707
  solvePart2(input) // 2207

}
