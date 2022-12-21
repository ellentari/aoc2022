package aoc

import aoc.algo.BFS

import scala.annotation.tailrec

object Day21 extends App {

  private val RootMonkey = "root"
  private val Human = "humn"

  type MonkeyName = String

  sealed trait MonkeyJob extends Product with Serializable
  object MonkeyJob {
    case class Yell(value: Int) extends MonkeyJob
    case class Plus(m1: MonkeyName, m2: MonkeyName) extends MonkeyJob
    case class Minus(m1: MonkeyName, m2: MonkeyName) extends MonkeyJob
    case class Multiply(m1: MonkeyName, m2: MonkeyName) extends MonkeyJob
    case class Divide(m1: MonkeyName, m2: MonkeyName) extends MonkeyJob
  }

  import MonkeyJob._

  def solvePart1(monkeyJobs: Map[MonkeyName, MonkeyJob]): Long =
    getMonkeyJobResult(RootMonkey, monkeyJobs)

  def solvePart2(monkeyJobs: Map[MonkeyName, MonkeyJob]): Option[Long] = {
    val root = monkeyJobs(RootMonkey).asInstanceOf[Plus]
    val dependentOnHuman = allDepending(Human, monkeyJobs)

    val (dependentMonkey, independentMonkey) =
      (dependentOnHuman.contains(root.m1), dependentOnHuman.contains(root.m2)) match {
        case (true, false) => (root.m1, root.m2)
        case (false, true) => (root.m2, root.m1)
        case _ => throw new RuntimeException("Unexpected state")
      }

    val target = getMonkeyJobResult(independentMonkey, monkeyJobs)

    findRequiredValue(target, dependentMonkey, dependentOnHuman.contains, monkeyJobs)
  }

  private def allDepending(on: MonkeyName, monkeyJobs: Map[MonkeyName, MonkeyJob]): Set[MonkeyName] = {
    def adjacentMonkeys(monkeyJob: MonkeyJob): List[MonkeyName] = monkeyJob match {
      case Yell(_) => Nil
      case Plus(m1, m2) => List(m1, m2)
      case Minus(m1, m2) => List(m1, m2)
      case Multiply(m1, m2) => List(m1, m2)
      case Divide(m1, m2) => List(m1, m2)
    }

    val adjacencyList = monkeyJobs
      .flatMap { case (monkey, job) =>
        adjacentMonkeys(job).map(_ -> monkey)
      }
      .toList
      .groupMap(_._1)(_._2)

    BFS.discoverRegion(on)(adjacencyList.getOrElse(_, Nil))
  }

  private def findRequiredValue(
    targetValue: Long,
    monkey: MonkeyName,
    dependsOnHuman: MonkeyName => Boolean,
    monkeyJobs: Map[MonkeyName, MonkeyJob]) = {

    @tailrec
    def loop(monkey: MonkeyName, target: Long): Option[Long] =
      monkeyJobs(monkey) match {
        case Yell(_) if monkey == Human =>
          Some(target)
        case Plus(m1, m2) if !dependsOnHuman(m1) =>
          val a = getMonkeyJobResult(m1, monkeyJobs)
          val b = target - a
          loop(m2, b)
        case Plus(m1, m2) if !dependsOnHuman(m2) =>
          val b = getMonkeyJobResult(m2, monkeyJobs)
          val a = target - b
          loop(m1, a)
        case Minus(m1, m2) if !dependsOnHuman(m1) =>
          val a = getMonkeyJobResult(m1, monkeyJobs)
          val b = a - target
          loop(m2, b)
        case Minus(m1, m2) if !dependsOnHuman(m2) =>
          val b = getMonkeyJobResult(m2, monkeyJobs)
          val a = target + b
          loop(m1, a)
        case Multiply(m1, m2) if !dependsOnHuman(m1) =>
          val a = getMonkeyJobResult(m1, monkeyJobs)
          val b = target / a
          loop(m2, b)
        case Multiply(m1, m2) if !dependsOnHuman(m2) =>
          val b = getMonkeyJobResult(m2, monkeyJobs)
          val a = target / b
          loop(m1, a)
        case Divide(m1, m2) if !dependsOnHuman(m1) =>
          val a = getMonkeyJobResult(m1, monkeyJobs)
          val b = a / target
          loop(m2, b)
        case Divide(m1, m2) if !dependsOnHuman(m2) =>
          val b = getMonkeyJobResult(m2, monkeyJobs)
          val a = target * b
          loop(m1, a)
        case _ => None
      }

    loop(monkey, targetValue)
  }

  private def getMonkeyJobResult(monkey: MonkeyName, monkeys: Map[MonkeyName, MonkeyJob]): Long = {
    def loop(monkey: String): Long = monkeys(monkey) match {
      case Yell(number) => number
      case Plus(m1, m2) => loop(m1) + loop(m2)
      case Minus(m1, m2) => loop(m1) - loop(m2)
      case Multiply(m1, m2) => loop(m1) * loop(m2)
      case Divide(m1, m2) => loop(m1) / loop(m2)
    }

    loop(monkey)
  }

  private def parseMonkey(string: String) = string match {
    case s"$monkey: $job" => monkey -> parseMonkeyJob(job)
  }

  private def parseMonkeyJob(string: String): MonkeyJob = string match {
    case s"$m1 + $m2" => Plus(m1, m2)
    case s"$m1 - $m2" => Minus(m1, m2)
    case s"$m1 * $m2" => Multiply(m1, m2)
    case s"$m1 / $m2" => Divide(m1, m2)
    case s"$number" => Yell(number.toInt)
  }

  private val sampleParsed = Input.asList("day21_sample.txt").map(parseMonkey).toMap
  private val inputParsed = Input.asList("day21.txt").map(parseMonkey).toMap

  println(solvePart1(sampleParsed)) // 152
  println(solvePart1(inputParsed)) // 194501589693264

  println(solvePart2(sampleParsed)) // 301
  println(solvePart2(inputParsed)) // 3887609741189

}
