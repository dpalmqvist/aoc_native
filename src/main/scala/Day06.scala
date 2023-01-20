package org.norrehem.aoc

import scala.io.Source

object Day06 extends App {

  def solution(row: String, num: Int): Int = {
    var index = num
    row.sliding(num).find {
      case chars: String if chars.toSet.size < num =>
        index = index + 1
        false
      case _ => true
    }
    index
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val rows = reader
    .getLines()
    .toSeq
  rows.foreach { row =>
    println(s"Problem 1: ${solution(row, 4)}")
    println(s"Problem 2: ${solution(row, 14)}")
  }

}
