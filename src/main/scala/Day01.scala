package org.norrehem.aoc

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class TopNStore(n: Int) {
  var store: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  def push(item: Int): Unit = {
    if (item > getMin) {
      store += item
      store = store.sorted
      if (store.size > n) {
        store = store.drop(1)
      }
    }
  }
  def getAll: Seq[Int] = store
  def getMin: Int = store.headOption.getOrElse(0)
}

object Day01 extends App {
  val NUM_TOP = 3
  val NUMERIC = raw"(\d+)".r
  val reader = Source.fromFile(args(0), "UTF-8")
  val topNStore = new TopNStore(NUM_TOP)
  val lastSum =
    reader.getLines().foldLeft((0)) {
      case (thisSum, NUMERIC(number)) =>
        thisSum + number.toInt
      case (thisSum, _) =>
        topNStore.push(thisSum)
        0
    }
  topNStore.push(lastSum)

  println(topNStore.getAll.sum)

}
