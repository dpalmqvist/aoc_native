package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App {
  case class Blizzard(
      initialPosition: (Int, Int),
      direction: Char,
      endPosition: (Int, Int),
      startPosition: (Int, Int)
  ) {
    val (initI, initJ) = initialPosition
    val (startI, startJ) = startPosition
    val (endI, endJ) = endPosition

    def at(time: Int): (Int, Int) = {
      direction match {
        case '>' => (Math.floorMod(initI - startI + time, endI) + startI, initJ)
        case '<' => (Math.floorMod(initI - startI - time, endI) + startI, initJ)
        case 'v' => (initI, Math.floorMod(initJ - startJ + time, endJ) + startJ)
        case '^' => (initI, Math.floorMod(initJ - startJ - time, endJ) + startJ)
      }
    }
  }

  def readBlizzards(rows: Seq[String]): Seq[Blizzard] = {
    rows.zipWithIndex.flatMap { case (row, j) =>
      row.zipWithIndex.flatMap {
        case ('#', _) | ('.', _) => Seq.empty
        case ('>', i)            => Seq(Blizzard((i, j), '>', (maxI, j), (1, j)))
        case ('<', i)            => Seq(Blizzard((i, j), '<', (maxI, j), (1, j)))
        case ('v', i)            => Seq(Blizzard((i, j), 'v', (i, maxJ), (i, 1)))
        case ('^', i)            => Seq(Blizzard((i, j), '^', (i, maxJ), (i, 1)))
        case ('2', i) =>
          Seq(
            Blizzard((i, j), '>', (maxI, j), (1, j)),
            Blizzard((i, j), 'v', (i, maxJ), (i, 1))
          )
      }
    }
  }

  def distanceToEnd(positionTime: ((Int, Int), Int)): Int = {
    val ((i, j), t) = positionTime
    val (endI, endJ) = endPosition
    Math.abs(endI - i) + Math.abs(endJ - j) + t
  }

  def getFollowerPositions(position: (Int, Int)): Seq[(Int, Int)] = {
    val (i, j) = position
    Seq(
      (i, j + 1),
      (i + 1, j),
      (i, j),
      (i, j - 1),
      (i - 1, j)
    ).filterNot { case (i, j) =>
      i == 0 || i == maxI + 1 ||
        (j < 0) || (j > maxJ + 1) ||
        ((j == 0) && (i != startPosition._1)) ||
        ((j == maxJ + 1) && (i != endPosition._1))
    }
  }

  @tailrec
  def dfs(openSet: Seq[((Int, Int), Int)], endPosition: (Int, Int)): Int = {
    if (openSet.nonEmpty && openSet.head._1 == endPosition) {
      openSet.head._2
    } else {
      val ((i, j), time) = openSet.head
      val blizzartPositions = blizzards.map(_.at(time + 1)).toSet
      val followerPositions =
        getFollowerPositions((i, j))
          .filterNot(blizzartPositions.contains)
          .map(position => (position, time + 1))
      val newOpenSet = (openSet.tail ++ followerPositions).distinct
        .sortBy(distanceToEnd)
      dfs(newOpenSet, endPosition)
    }

  }

  val reader = Source.fromFile(args(0), "UTF-8")

  val rows = reader
    .getLines()
    .toSeq

  val maxI = rows.head.length - 2
  val maxJ = rows.size - 2

  val startTime = System.currentTimeMillis()
  val startPosition = (rows.head.indexOf('.'), 0)
  val endPosition = (rows.last.indexOf('.'), maxJ + 1)
  val blizzards = readBlizzards(rows)
  val bestTimeForward = dfs(Seq((startPosition, 0)), endPosition)
  println(s"Problem1: ${bestTimeForward}")
  val bestTimeReverse = dfs(Seq((endPosition, bestTimeForward)), startPosition)
  val bestForwardAgain = dfs(Seq((startPosition, bestTimeReverse)), endPosition)
  println(s"Problem2: $bestForwardAgain")
  println(s"Total time: ${System.currentTimeMillis() - startTime}")

}
