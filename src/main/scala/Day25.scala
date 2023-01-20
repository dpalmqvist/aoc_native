package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day25 extends App {

  @tailrec
  def addSnafuDigits(d1: Char, d2: Char): Seq[Char] = {
    (d1, d2) match {
      case ('=', '=') => "-1"
      case ('=', '-') => "-2"
      case ('0', _)   => s"0${d2}"
      case (_, '0')   => s"0${d1}"
      case ('=', '1') => "0-"
      case ('=', '2') => "00"
      case ('-', '-') => "0="
      case ('-', '1') => "00"
      case ('-', '2') => "01"
      case ('1', '1') => "02"
      case ('1', '2') => "1="
      case ('2', '2') => "1-"
      case _          => addSnafuDigits(d2, d1)
    }
  }

  def addSnafu(snafu1: String, snafu2: String): String = {
    @tailrec
    def recur(
        snafu1: String,
        snafu2: String,
        carry: Char,
        result: String
    ): String = {
      if (snafu1.isEmpty && snafu2.isEmpty && carry == '0') {
        result
      } else {
        val lastDigit1 = if (snafu1.nonEmpty) snafu1.last else '0'
        val lastDigit2 = if (snafu2.nonEmpty) snafu2.last else '0'
        val numberDigitSum = addSnafuDigits(lastDigit1, lastDigit2)
        val withCarrySum = addSnafuDigits(numberDigitSum.last, carry)
        val carrySum =
          addSnafuDigits(numberDigitSum.head, withCarrySum.head).last
        recur(snafu1.init, snafu2.init, carrySum, withCarrySum.last +: result)
      }
    }
    recur(snafu1, snafu2, '0', "")
  }

  val reader = Source.fromFile(args(0), "UTF-8")

  val numbers = reader
    .getLines()
    .toSeq

  val snafuSum = numbers.foldLeft("0") { case (acc, snafu) =>
    addSnafu(acc, snafu)
  }
  println(s"Problem1: $snafuSum")

}
