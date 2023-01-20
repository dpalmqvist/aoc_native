package org.norrehem.aoc

object Day01_golf extends App {
  val NUM_TOP = 3
  val util = new Util[Int](args(0), "", itemParser = _.toInt)
  val lastSum = util.getGroups
    .map(_.sum)
    .sortBy(x => -x)
    .take(NUM_TOP)
    .sum

  println(lastSum)

}
