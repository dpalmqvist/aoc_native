package org.norrehem.aoc

import scala.io.Source

object Day03 extends App {
  def getPriority(c: Char) = {
    if ('a' <= c && c <= 'z') {
      1 + (c - 'a')
    } else {
      27 + (c - 'A')
    }
  }

  def getCompartments(rucksack: String): Seq[String] = {
    val size = rucksack.length
    Seq(rucksack.substring(0, size / 2), rucksack.substring(size / 2, size))
  }

  def getCommonItems(contents: Seq[String]): Set[Char] = {
    val result = contents.tail.foldLeft(contents.head.toCharArray.toSet) {
      case (acc, rucksack) => acc.intersect(rucksack.toCharArray.toSet)
    }
    result
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val rows = reader
    .getLines()
    .toSeq
  val problem1 = rows
    .map(row => getCommonItems(getCompartments(row)).map(getPriority).sum)
    .sum
  val problem2 = rows
    .sliding(3, 3)
    .map(rowset => //  kk
      getCommonItems(rowset).map(getPriority).sum
    )
    .sum
  println(s"Problem1 $problem1")
  println(s"Problem2 $problem2")
}
