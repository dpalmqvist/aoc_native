package org.norrehem.aoc

import scala.io.Source

object Day04 extends App {
  case class IntRange(start: Int, end: Int) {
    def isEmpty: Boolean = end < start
    def intersection(other: IntRange): IntRange = {
      val newStart = if (start < other.start) {
        other.start
      } else {
        start
      }
      val newEnd = if (end > other.end) {
        other.end
      } else {
        end
      }
      IntRange(newStart, newEnd)
    }
  }

  val RANGE_RE = raw"(\d+)-(\d+),(\d+)-(\d+)".r
  val reader = Source.fromFile(args(0), "UTF-8")
  val rows = reader
    .getLines()
    .toSeq
  val problem1 = rows.count { case RANGE_RE(s1, e1, s2, e2) =>
    val rangeLeft = IntRange(s1.toInt, e1.toInt)
    val rangeRight = IntRange(s2.toInt, e2.toInt)
    val intersection = rangeLeft.intersection(rangeRight)
    (intersection == rangeLeft) || (intersection == rangeRight)
  }
  val problem2 = rows.count { case RANGE_RE(s1, e1, s2, e2) =>
    val rangeLeft = IntRange(s1.toInt, e1.toInt)
    val rangeRight = IntRange(s2.toInt, e2.toInt)
    val intersection = rangeLeft.intersection(rangeRight)
    !intersection.isEmpty
  }
  println(s"Problem1 $problem1")
  println(s"Problem2 $problem2")
}
