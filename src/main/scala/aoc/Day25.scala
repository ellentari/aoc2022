package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.pow

object Day25 extends App {

  sealed abstract class SNAFUDigit(val intValue: Int, val charValue: Char)
    extends Product with Serializable

  object SNAFUDigit {
    case object `2` extends SNAFUDigit(2, '2')
    case object `1` extends SNAFUDigit(1, '1')
    case object `0` extends SNAFUDigit(0, '0')
    case object `-` extends SNAFUDigit(-1, '-')
    case object `=` extends SNAFUDigit(-2, '=')

    def values: List[SNAFUDigit] = List(`2`, `1`, `0`, `-`, `=`)

    def fromChar(char: Char): SNAFUDigit =
      values.find(_.charValue == char)
        .getOrElse(throw new IllegalArgumentException(s"Invalid SNAFU digit: $char"))
  }

  case class SNAFUNumber(digits: List[SNAFUDigit]) extends AnyVal {

    def toDecimal: Long =
      digits
        .reverse
        .foldLeft((0, 0L)) { case ((exponent, acc), digit) =>
          val powerOf5 = pow(5, exponent).toLong

          (exponent + 1, acc + (powerOf5 * digit.intValue))
        }._2

    override def toString: String = digits.view.map(_.charValue).mkString("")
  }

  object SNAFUNumber {
    def fromString(string: String): SNAFUNumber =
      SNAFUNumber(string.view.map(SNAFUDigit.fromChar).toList)

    def fromDecimal(decimal: Long): SNAFUNumber = {
      @tailrec
      def loop(remainingNumber: Long, acc: mutable.ListBuffer[SNAFUDigit]): Unit =
        if (remainingNumber > 0) {
          val remainder = remainingNumber % 5
          val toAdd = if (remainder > 2) 1 else 0

          val snafuDigit = remainder match {
            case 0 => SNAFUDigit.`0`
            case 1 => SNAFUDigit.`1`
            case 2 => SNAFUDigit.`2`
            case 3 => SNAFUDigit.`=`
            case 4 => SNAFUDigit.`-`
          }

          acc.addOne(snafuDigit)

          loop(remainingNumber / 5 + toAdd, acc)
        }

      val result = mutable.ListBuffer.empty[SNAFUDigit]

      loop(decimal, result)

      SNAFUNumber(result.reverse.toList)
    }
  }

  def solvePart1(numbers: List[SNAFUNumber]): SNAFUNumber =
    SNAFUNumber.fromDecimal(numbers.map(_.toDecimal).sum)

  private val sample = Input.asList("day25_sample.txt").map(SNAFUNumber.fromString)
  private val input = Input.asList("day25.txt").map(SNAFUNumber.fromString)

  println(solvePart1(sample)) // 2=-1=0
  println(solvePart1(input)) // 2=2-1-010==-0-1-=--2

}
