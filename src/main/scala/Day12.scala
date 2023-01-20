package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App {
  def getPath(
      flow: Map[(Int, Int), (Int, Option[(Int, Int)])],
      heights: Map[(Int, Int), Char]
  ): Seq[(Int, Int)] = {
    val endCoord = heights.find(_._2 == 'E').head._1
    val startCoord = heights.find(_._2 == 'S').head._1
    val maxI = flow.keys.map(_._1).max
    val maxJ = flow.keys.map(_._2).max

    @tailrec
    def recur(path: Seq[(Int, Int)]): Seq[(Int, Int)] = {
      if (path.last == startCoord) {
        path
      } else {
        recur(path ++ flow(path.last)._2)
      }
    }
    recur(Seq(endCoord)).reverse
  }
  def printFlowChart(flow: Map[(Int, Int), (Int, Option[(Int, Int)])]): Unit = {
    val maxI = flow.keys.map(_._1).max
    val maxJ = flow.keys.map(_._2).max
    (0 to maxJ).foreach { j =>
      val row = (0 to maxI)
        .map { i =>
          flow((i, j))._2 match {
            case None                           => "."
            case Some((ii, j)) if (ii == i - 1) => "<"
            case Some((ii, j)) if (ii == i + 1) => ">"
            case Some((i, jj)) if (jj == j - 1) => "^"
            case Some((i, jj)) if (jj == j + 1) => "v"
            case _                              => "*"
          }
        }
        .mkString("")
      println(row)
    }
  }

  def neighbours(
      coords: (Int, Int),
      unvisited: Set[(Int, Int)]
  ): Seq[(Int, Int)] = {
    coords match {
      case (i, j) =>
        Seq((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1))
          .filter(unvisited.contains)
    }
  }

  def allowedStep1(hFrom: Char, hTo: Char): Boolean = {
    (((hFrom != 'S') && (hTo != 'E') && (hTo - hFrom) <= 1)) ||
    (hFrom == 'S' && (hTo - 'a') <= 1) ||
    (hTo == 'E' && ('z' - hFrom) <= 1)
  }

  @tailrec
  def dijkstra(
      heights: Map[(Int, Int), Char],
      distPrev: Map[(Int, Int), (Int, Option[(Int, Int)])],
      unvisited: Set[(Int, Int)],
      allowedStep: (Char, Char) => Boolean
  ): Map[(Int, Int), (Int, Option[(Int, Int)])] = {
    if (unvisited.isEmpty) {
      distPrev
    } else {
      val u = unvisited
        .map(coordinates => (coordinates, distPrev(coordinates)._1))
        .minBy(_._2)
        ._1
      val newUnvisited = unvisited - u
      val newDistPrev = neighbours(u, unvisited)
        .foldLeft(distPrev) {
          case (acc, neighbor)
              if allowedStep(heights(u), heights(neighbor)) &&
                (acc(u)._1 + 1 < acc(neighbor)._1) =>
            acc.updated(neighbor, (acc(u)._1 + 1, Some(u)))
          case (acc, _) => acc
        }
      dijkstra(heights, newDistPrev, newUnvisited, allowedStep)
    }
  }

  private def findShortestPath(
      heights: Map[(Int, Int), Char],
      allowedStep: (Char, Char) => Boolean
  ) = {
    val distPrev = heights.map {
      case (coords, 'S') => (coords, (0, None))
      case (coords, _)   => (coords, (Int.MaxValue - 10, None))
    }
    val unvisited = heights.keySet
    dijkstra(heights, distPrev, unvisited, allowedStep)
  }
  val startTime = System.currentTimeMillis()
  val reader = Source.fromFile(args(0), "UTF-8")
  val heights = reader
    .getLines()
    .toSeq
    .zipWithIndex
    .flatMap { case (row, j) =>
      row.zipWithIndex.map { case (height, i) =>
        ((i, j), height)
      }
    }
    .toMap

  val startSquare = heights.filter(_._2 == 'S').head._1
  val endSquare = heights.filter(_._2 == 'E').head._1
  val flowChart = findShortestPath(heights, allowedStep1)
  val path = getPath(flowChart, heights)
  val problem1Time = System.currentTimeMillis()
  println(s"Problem1: ${flowChart(endSquare)} ${problem1Time - startTime}")
  def allowedStep2(hFrom: Char, hTo: Char): Boolean = {
    (((hFrom != 'S') && (hFrom - hTo) <= 1)) ||
    (hFrom == 'S' && ('z' - hTo) <= 1)
  }
  val aSquares = heights.filter(sq => sq._2 == 'a' || sq._2 == 'S').keys
  val heights2 = heights.updated(startSquare, 'a').updated(endSquare, 'S')
  val flowChart2 = findShortestPath(heights2, allowedStep2)
  val aDistances = aSquares.map(flowChart2)
  val problem2Time = System.currentTimeMillis()

  println(
    s"Problem2: ${aDistances.minBy(_._1)} ${problem2Time - problem1Time}"
  )
}
