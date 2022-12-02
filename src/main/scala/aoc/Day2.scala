package aoc

object Day2 extends App {

  case class Your[A](value: A) extends AnyVal {
    def map[B](f: A => B): Your[B] = Your(f(value))
  }

  case class Opponent[A](value: A) extends AnyVal {
    def map[B](f: A => B): Opponent[B] = Opponent(f(value))
  }

  object RockPaperScissors {

    sealed abstract class Shape(val score: Int) extends Product with Serializable { self =>

      def winsOver: Shape = self match {
        case Shape.Rock => Shape.Scissors
        case Shape.Paper => Shape.Rock
        case Shape.Scissors => Shape.Paper
      }

      def losesTo: Shape = self match {
        case Shape.Rock => Shape.Paper
        case Shape.Paper => Shape.Scissors
        case Shape.Scissors => Shape.Rock
      }

    }

    object Shape {
      case object Rock extends Shape(1)
      case object Paper extends Shape(2)
      case object Scissors extends Shape(3)
    }

    sealed abstract class Outcome(val score: Int) extends Product with Serializable
    object Outcome {
      case object Win extends Outcome(6)
      case object Draw extends Outcome(3)
      case object Lose extends Outcome(0)
    }

    case class Round(you: Your[Shape], opponent: Opponent[Shape], yourOutcome: Your[Outcome])

    import Outcome._

    def calculateOutcome(your: Your[Shape], opponent: Opponent[Shape]): Your[Outcome] =
      your.map {
        case s if s == opponent.value => Draw
        case s => if (s.winsOver == opponent.value) Win else Lose
      }

    def calculateShapeFromOutcome(opponent: Opponent[Shape], outcome: Your[Outcome]): Your[Shape] =
      outcome.map {
        case Win => opponent.value.losesTo
        case Draw => opponent.value
        case Lose => opponent.value.winsOver
      }
  }

  import RockPaperScissors._
  import RockPaperScissors.Shape._
  import RockPaperScissors.Outcome._

  object RockPaperScissorsParser {

    def parseTwoShapes: String => (Opponent[Shape], Your[Shape]) =
      parsePair[Opponent[Shape], Your[Shape]](
        (parseABCAsShape _).andThen(Opponent(_)),
        (parseXYZAsShape _).andThen(Your(_))
      )

    def parseShapeAndOutcome: String => (Opponent[Shape], Your[Outcome]) =
      parsePair[Opponent[Shape], Your[Outcome]](
        (parseABCAsShape _).andThen(Opponent(_)),
        (parseXYZAsOutcome _).andThen(Your(_))
      )

    private def parseABCAsShape(raw: String) = raw match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
    }

    private def parseXYZAsShape(raw: String) = raw match {
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissors
    }

    private def parseXYZAsOutcome(raw: String) = raw match {
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
    }

    private def parsePair[A, B](
      parseFirstColumn: String => A,
      parseSecondColumn: String => B)(
      input: String
    ): (A, B) =
      input match {
        case s"$a $b" =>
          val first = parseFirstColumn(a)
          val second = parseSecondColumn(b)

          (first, second)
      }
  }

  def solvePart1: List[(Opponent[Shape], Your[Shape])] => Int =
    solve[Your[Shape]] { case (opponent, you) =>
      Round(you, opponent, calculateOutcome(you, opponent))
    }

  def solvePart2: List[(Opponent[Shape], Your[Outcome])] => Int =
    solve[Your[Outcome]] { case (opponent, yourOutcome) =>
      Round(calculateShapeFromOutcome(opponent, yourOutcome), opponent, yourOutcome)
    }

  private def solve[A](toRound: (Opponent[Shape], A) => Round)(input: List[(Opponent[Shape], A)]) =
    input
      .map { case (opponent, a) =>
        val round = toRound(opponent, a)

        round.you.value.score + round.yourOutcome.value.score
      }
      .sum

  private val sample =
    """A Y
      |B X
      |C Z""".stripMargin.split("\n").toList

  private val input = Input.asList("day2.txt")

  println(solvePart1(sample.map(RockPaperScissorsParser.parseTwoShapes))) // 15
  println(solvePart1(input.map(RockPaperScissorsParser.parseTwoShapes))) // 15422

  println(solvePart2(sample.map(RockPaperScissorsParser.parseShapeAndOutcome))) // 12
  println(solvePart2(input.map(RockPaperScissorsParser.parseShapeAndOutcome))) // 15442

}
