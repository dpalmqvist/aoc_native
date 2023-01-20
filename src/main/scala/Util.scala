package org.norrehem.aoc

import scala.io.Source

class Util[T](
    inputPath: String,
    groupSeparator: String,
    itemSeparator: String = "§",
    itemParser: String => T
) {
  private val reader = Source.fromFile(inputPath, "UTF-8")
  private val data = reader.getLines().mkString(itemSeparator)
  def getGroups: List[List[T]] = {
    val items = data
      .split(itemSeparator)
    items
      .foldLeft(List.empty[List[T]]) {
        case (acc, `groupSeparator`) => List.empty[T] :: acc
        case ((head :: tail), value) => (itemParser(value) :: head) :: tail
        case (List(), value)         => List(List(itemParser(value)))
      }
  }
}
