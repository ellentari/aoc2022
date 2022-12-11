package aoc

object Day11 extends App {

  type MonkeyId = Int
  type WorryLevel = Long
  type MonkeyBusiness = Int

  case class Item(worryLevel: WorryLevel)

  case class Monkey(id: MonkeyId, impactWorry: WorryLevel => WorryLevel, test: MonkeyDivisibilityTest) {

    def throwItem(
      monkeyItems: Map[MonkeyId, List[Item]],
      manageWorry: WorryLevel => WorryLevel,
      item: Item): Map[MonkeyId, List[Item]] = {
      val increasedWorry = impactWorry(item.worryLevel)
      val managedWorry = manageWorry(increasedWorry)
      val throwTo = test.shouldThrowTo(managedWorry)

      monkeyItems
        .updatedWith(id)(_.map(_.filterNot(_ eq item)))
        .updatedWith(throwTo)(items => Some(items.getOrElse(Nil) :+ item.copy(worryLevel = managedWorry)))
    }
  }

  case class MonkeyDivisibilityTest(divisor: WorryLevel, ifTrueThrowTo: MonkeyId, ifFalseThrowTo: MonkeyId) {

    def shouldThrowTo(worryLevel: WorryLevel): MonkeyId =
      if (worryLevel % divisor == 0) ifTrueThrowTo else ifFalseThrowTo
  }

  case class State(
    monkeyItems: Map[MonkeyId, List[Item]],
    monkeyBusiness: Map[MonkeyId, MonkeyBusiness] = Map.empty)

  def solvePart1: List[(Monkey, List[Item])] => Long = solve(_ / 3, rounds = 20)

  def solvePart2(input: List[(Monkey, List[Item])]): Long = {
    val mod = input.map(_._1.test.divisor).product
    solve(_ % mod, rounds = 10_000)(input)
  }

  private def solve(manageWorry: WorryLevel => WorryLevel, rounds: Int)(input: List[(Monkey, List[Item])]): Long = {
    val monkeys = input.map(_._1)

    val initialState = State(input.map { case (m, items) => m.id -> items }.toMap)
    val finalState = (0 until rounds)
      .foldLeft(initialState)((state, _) => throwItems(state, monkeys, manageWorry))

    finalState.monkeyBusiness.values.toList
      .sortBy(-_)
      .take(2)
      .map(_.toLong)
      .product
  }

  private def throwItems(state: State, monkeys: List[Monkey], manageWorry: WorryLevel => WorryLevel): State = {
    def throwItems(state: State, monkey: Monkey): State = {
      val itemsToThrow = state.monkeyItems.getOrElse(monkey.id, Nil)
      val updMonkeyItems = itemsToThrow.foldLeft(state.monkeyItems)(monkey.throwItem(_, manageWorry, _))

      val updMonkeyBusiness = state.monkeyBusiness
        .updatedWith(monkey.id)(v => Some(v.getOrElse(0) + itemsToThrow.size))

      State(updMonkeyItems, updMonkeyBusiness)
    }

    monkeys.foldLeft(state)(throwItems)
  }

  private def parseInput(input: String) =
    input
      .split("\n\n")
      .map(_.split("\n").toVector)
      .map(parseMonkey)
      .toList

  private def parseMonkey(raw: Vector[String]): (Monkey, List[Item]) = {
    val startingItems = parseStartingItems(raw(1))
    val monkey = Monkey(
      parseMonkeyId(raw(0)),
      parseOperation(raw(2)),
      parseMonkeyTest(raw.slice(3, 6))
    )

    (monkey, startingItems)
  }

  private def parseMonkeyId(raw: String): MonkeyId = raw match {
    case s"Monkey $id:" => id.toInt
  }

  private def parseStartingItems(raw: String): List[Item] = raw.trim match {
    case s"Starting items: $items" => items.split(", ").map(_.toLong).map(Item).toList
  }

  private def parseOperation(raw: String): WorryLevel => WorryLevel = {
    def parse(operand: String, f: (WorryLevel, WorryLevel) => WorryLevel) =
      operand match {
        case "old" => (wl: WorryLevel) => f(wl, wl)
        case value =>
          val valueInt = value.toInt
          (wl: WorryLevel) => f(wl, valueInt)
      }

    raw.trim match {
      case s"Operation: new = old $op $value" =>
        op match {
          case "+" => parse(value, _ + _)
          case "*" => parse(value, _ * _)
        }
    }
  }

  private def parseMonkeyTest(lines: Vector[String]): MonkeyDivisibilityTest = {
    def parseThrowToMonkeyCondition(expectedCondition: String, raw: String): MonkeyId = raw.trim match {
      case s"If $condition: throw to monkey $id" if condition == expectedCondition =>
        id.toInt
    }

    val divisor = lines(0).trim match {
      case s"Test: divisible by $value" => value.toInt
    }

    MonkeyDivisibilityTest(
      divisor,
      parseThrowToMonkeyCondition("true", lines(1)),
      parseThrowToMonkeyCondition("false", lines(2))
    )
  }

  private val sampleParsed = parseInput(Input.asString("day11_sample.txt"))
  private val inputParsed = parseInput(Input.asString("day11.txt"))

  println(solvePart1(sampleParsed)) // 10605
  println(solvePart1(inputParsed)) // 121450
  println(solvePart2(sampleParsed)) // 2713310158
  println(solvePart2(inputParsed)) // 28244037010

}
