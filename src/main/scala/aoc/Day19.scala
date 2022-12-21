package aoc

import scala.collection.mutable

object Day19 extends App {

  case class Cost(ore: Int, clay: Int = 0, obsidian: Int = 0)

  case class Blueprint(id: Int, oreRobot: Cost, clayRobot: Cost, obsidian: Cost, geodeRobot: Cost) {
    val maxOreCost: Int = allCosts.map(_.ore).max
    val maxClayCost: Int = allCosts.map(_.clay).max
    val maxObsidianCost: Int = allCosts.map(_.obsidian).max

    private def allCosts: List[Cost] = List(oreRobot, clayRobot, obsidian, geodeRobot)

  }

  case class State(
    remainingMinutes: Int = 0,
    ore: Int = 0,
    clay: Int = 0,
    obsidian: Int = 0,
    geode: Int = 0,
    oreRobots: Int = 0,
    clayRobots: Int = 0,
    obsidianRobots: Int = 0,
    geodeRobots: Int = 0) {

    def enoughResources(cost: Cost): Boolean =
      ore >= cost.ore &&
        clay >= cost.clay &&
        obsidian >= cost.obsidian

    def tickTime: State =
      copy(remainingMinutes = remainingMinutes - 1)

    def increaseResources: State =
      copy(
        ore = ore + oreRobots,
        clay = clay + clayRobots,
        obsidian = obsidian + obsidianRobots,
        geode = geode + geodeRobots
      )

    def minus(cost: Cost): State =
      copy(
        ore = ore - cost.ore,
        clay = clay - cost.clay,
        obsidian = obsidian - cost.obsidian
      )

    def buildOreRobot(cost: Cost): State =
      minus(cost).copy(oreRobots = oreRobots + 1)

    def buildClayRobot(cost: Cost): State =
      minus(cost).copy(clayRobots = clayRobots + 1)

    def buildObsidianRobot(cost: Cost): State =
      minus(cost).copy(obsidianRobots = obsidianRobots + 1)

    def buildGeodeRobot(cost: Cost): State =
      minus(cost).copy(geodeRobots = geodeRobots + 1)

  }

  def solvePart1(input: List[Blueprint]): Int =
    input.map(blueprint => blueprint.id * solve(blueprint, minutes = 24)).sum

  def solvePart2(input: List[Blueprint]): Int =
    input.take(3).map(solve(_, minutes = 32)).product

  private def solve(blueprint: Blueprint, minutes: Int): Int = {
    var maxResult = 0

    val queue = mutable.Queue.empty[State]
    val processed = mutable.HashSet.empty[State]

    queue.enqueue(State(remainingMinutes = minutes, oreRobots = 1))

    while (queue.nonEmpty) {
      val stateRaw = queue.dequeue()
      maxResult = maxResult max stateRaw.geode

      if (stateRaw.remainingMinutes > 0) {
        val initialState = cutState(stateRaw, blueprint)

        if (!processed.contains(initialState)) {

          processed.add(initialState)

          if (processed.size % 1_000_000 == 0)
            println(s"T: ${stateRaw.remainingMinutes}; max = $maxResult; processed: ${processed.size}; in queue: ${queue.size}")

          val nextStateBase = initialState.increaseResources.tickTime

          queue.enqueue(nextStateBase)

          if (initialState.enoughResources(blueprint.oreRobot))
            queue.append(nextStateBase.buildOreRobot(blueprint.oreRobot))

          if (initialState.enoughResources(blueprint.clayRobot))
            queue.append(nextStateBase.buildClayRobot(blueprint.clayRobot))

          if (initialState.enoughResources(blueprint.obsidian))
            queue.append(nextStateBase.buildObsidianRobot(blueprint.obsidian))

          if (initialState.enoughResources(blueprint.geodeRobot))
            queue.append(nextStateBase.buildGeodeRobot(blueprint.geodeRobot))
        }
      }
    }

    maxResult
  }

  private def cutState(state: State, blueprint: Blueprint) = {
    val oreRobots = state.oreRobots min blueprint.maxOreCost
    val clayRobots = state.clayRobots min blueprint.maxClayCost
    val obsidianRobots = state.obsidianRobots min blueprint.maxObsidianCost

    val t = state.remainingMinutes
    val ore = state.ore min (t * blueprint.maxOreCost - (t - 1) * oreRobots)
    val clay = state.clay min (t * blueprint.maxClayCost - (t - 1) * clayRobots)
    val obsidian = state.obsidian min (t * blueprint.maxObsidianCost - (t - 1) * obsidianRobots)

    State(
      t,
      ore,
      clay,
      obsidian,
      state.geode,
      oreRobots,
      clayRobots,
      obsidianRobots,
      state.geodeRobots)
  }

  private def parseBlueprint(raw: String): Blueprint = {

    def parseBlueprintId(s: String) = s match {
      case s"Blueprint $id" => id.toInt
    }

    def parseOreRobotCost(s: String) = s match {
      case s"Each ore robot costs $ore ore" =>
        Cost(ore.toInt)
    }

    def parseClayRobotCost(s: String) = s match {
      case s"Each clay robot costs $ore ore" =>
        Cost(ore.toInt)
    }

    def parseObsidianRobotCost(s: String) = s match {
      case s"Each obsidian robot costs $ore ore and $clay clay" =>
        Cost(ore.toInt, clay = clay.toInt)
    }

    def parseGeodeRobotCost(s: String) = s match {
      case s"Each geode robot costs $ore ore and $obsidian obsidian" =>
        Cost(ore.toInt, obsidian = obsidian.toInt)
    }

    val Array(blueprint, content) = raw.split(": ")
    val costs = content.split("\\.").map(_.trim)

    Blueprint(
      parseBlueprintId(blueprint),
      parseOreRobotCost(costs(0)),
      parseClayRobotCost(costs(1)),
      parseObsidianRobotCost(costs(2)),
      parseGeodeRobotCost(costs(3))
    )
  }

  private val sample = Input.asList("day19_sample.txt").map(parseBlueprint)
  private val input = Input.asList("day19.txt").map(parseBlueprint)

  println(solvePart1(sample)) // 33
  println(solvePart1(input)) // 1356

  println(solvePart2(sample)) // 3472
  println(solvePart2(input)) // 27720

}
