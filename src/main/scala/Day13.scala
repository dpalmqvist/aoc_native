package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {
  val facit =
    "1, 4, 7, 9, 10, 12, 13, 16, 17, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 31, 34, 35, 37, 38, 39, 43, 49, 50, 51, 52, 53, 54, 56, 57, 60, 61, 64, 66, 67, 69, 74, 75, 77, 81, 82, 83, 87, 88, 89, 91, 95, 97, 98, 99, 105, 106, 108, 109, 113, 115, 117, 118, 125, 126, 127, 128, 132, 134, 135, 137, 138, 140, 141, 147, 148, 149"
      .split(",")
      .map(_.trim.toInt)
  trait Comparable {
    def compare(other: Comparable): Int
  }
  case class Value(value: Int) extends Comparable {
    override def compare(other: Comparable): Int = {
      other match {
        case other: Value =>
          if (this.value < other.value) { -1 }
          else if (this.value > other.value) { +1 }
          else 0
        case Arr(empty) if empty.isEmpty => +1
        case Arr(values)                 => Arr(Seq(this)).compare(other)
      }
    }
  }
  case class Arr(arr: Seq[Comparable]) extends Comparable {
    @tailrec
    private def compareArrays(
        arrLeft: Seq[Comparable],
        arrRight: Seq[Comparable]
    ): Int = {
      (arrLeft, arrRight) match {
        case (empty, alsoEmpty) if (empty.isEmpty && alsoEmpty.isEmpty) => 0
        case (empty, nonEmpty) if (nonEmpty.nonEmpty && empty.isEmpty)  => -1
        case (nonEmpty, empty) if (nonEmpty.nonEmpty && empty.isEmpty)  => 1
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          leftHead.compare(rightHead) match {
            case -1 => -1
            case 1  => +1
            case 0  => compareArrays(leftTail, rightTail)
          }

      }
    }
    override def compare(other: Comparable): Int = {
      other match {
        case Value(_)      => this.compare(Arr(Seq(other)))
        case Arr(otherArr) => compareArrays(this.arr, otherArr)
      }

    }
  }
  def getArr(arrString: String): Arr = {
    val (values, ongoing, _) = arrString
      .substring(1, arrString.length - 1)
      .foldLeft(Seq.empty[Comparable], "", 0) {
        case ((acc, ongoing, 0), ',')
            if ongoing.startsWith("[") && ongoing.endsWith("]") =>
          (acc :+ getArr(ongoing), "", 0)
        case ((acc, ongoing, 0), ',') =>
          (acc :+ Value(value = ongoing.toInt), "", 0)
        case ((acc, ongoing, depth), '[') => (acc, ongoing + '[', depth + 1)
        case ((acc, ongoing, depth), ']') => (acc, ongoing + ']', depth - 1)
        case ((acc, ongoing, depth), c)   => (acc, ongoing + c, depth)
      }
    if (ongoing.startsWith("[")) {
      Arr(values :+ getArr(ongoing))
    } else if (ongoing.isEmpty) {
      Arr(Seq.empty)
    } else {
      Arr(values :+ Value(ongoing.toInt))
    }
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val arrayPairs = reader
    .getLines()
    .toSeq
    .sliding(2, 3)
    .toSeq
  val result1 = arrayPairs.zipWithIndex
    .map { case (rowPair, index) =>
      val leftArr = getArr(rowPair.head)
      val rightArr = getArr(rowPair.last)
      (index + 1, leftArr.compare(rightArr))
    }
  result1.foreach { case (index, result) =>
    println(
      s"$index -> $result",
      if (
        (result == -1 && !facit.contains(index)) || (result == 1 && facit
          .contains(index))
      ) "ERROR"
      else "OK"
    )
  }
  val answer1 =
    result1
      .filter(_._2 == -1)
      .map(_._1)
      .sum
  println(s"Problem1: $answer1")
  val dividers = Seq("[[2]]", "[[6]]")
  val allArrays: Seq[String] = dividers ++ arrayPairs.flatten
  val sorted = allArrays.sortWith { case (left, right) =>
    val leftArr = getArr(left)
    val rightArr = getArr(right)
    leftArr.compare(rightArr) == -1
  }
  println(s"Problem2: ${sorted.zipWithIndex
    .filter { case (row, index) =>
      dividers.contains(row)
    }
    .map(_._2 + 1)
    .product}")
}
