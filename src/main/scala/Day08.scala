package org.norrehem.aoc

import scala.io.Source

object Day08 extends App {
  case class Woods(trees: Map[(Int, Int), Int], maxI: Int, maxJ: Int) {
    def getLeftOf(
        i: Int,
        j: Int
    ): Seq[Int] =
      (0 until i).map(ii => trees((ii, j))).reverse
    def getRightOf(
        i: Int,
        j: Int
    ): Seq[Int] =
      (i + 1 to maxI).map(ii => trees((ii, j)))
    def getAbove(
        i: Int,
        j: Int
    ): Seq[Int] =
      (0 until j).map(jj => trees((i, jj))).reverse
    def getBelow(
        i: Int,
        j: Int
    ): Seq[Int] =
      (j + 1 to maxJ).map(jj => trees((i, jj)))

    def isVisible(i: Int, j: Int): Boolean = {
      Seq(
        getLeftOf(i, j),
        getAbove(i, j),
        getRightOf(i, j),
        getBelow(i, j)
      ).exists(_.forall(_ < trees((i, j))))
    }

    def sight(heights: Seq[Int], height: Int, maxSize: Int): Int = {
      heights.zipWithIndex.find(_._1 >= height).map(_._2 + 1).getOrElse(maxSize)
    }

    def score(i: Int, j: Int): Int = {
      Seq(
        (getLeftOf(i, j), i),
        (getRightOf(i, j), maxI - i),
        (getAbove(i, j), j),
        (getBelow(i, j), maxJ - j)
      ).map { case (heights, maxDist) =>
        sight(heights, trees(i, j), maxDist)
      }.product
    }

  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val rows = reader
    .getLines()
    .toSeq
  val trees: Map[(Int, Int), Int] =
    rows.zipWithIndex.foldLeft(Map.empty[(Int, Int), Int]) {
      case (acc, (row, j)) =>
        row.zipWithIndex.foldLeft(acc) { case (accInner, (height, i)) =>
          accInner.updated((i, j), height.getNumericValue)
        }
    }
  val (maxI: Int, maxJ: Int) = trees.keySet.maxBy { case (i, j) => i * j }
  val woods: Woods = Woods(trees, maxI, maxJ)
  val problem1: Int = woods.trees.keySet.count { case (i, j) =>
    woods.isVisible(i, j)
  }
  val problem2: Int = woods.trees.keySet.map { case (i, j) =>
    woods.score(i, j)
  }.max
  println(s"Problem1: $problem1")
  println(s"Problem2: $problem2")

}
