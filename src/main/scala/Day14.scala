package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {
  val COORD_RE = raw"(\d+),(\d+)".r
  def printBoard(board: Map[(Int, Int), Char]): Unit = {
    val xs = board.keys.map(_._1)
    val ys = board.keys.map(_._2)
    val minX = xs.min
    val maxX = xs.max
    val minY = ys.min
    val maxY = ys.max
    (minY to maxY).foreach { y =>
      val row =
        (minX to maxX).map(x => board.getOrElse((x, y), '.')).mkString("")
      println(f"$y%4d - $row")
    }
  }

  def getWalls(row: String): List[((Int, Int), Char)] = {
    val endPoints = row
      .split("->")
      .map(_.trim match {
        case COORD_RE(x, y) => (x.toInt, y.toInt)
      })
    endPoints
      .sliding(2)
      .flatMap {
        _.toList match {
          case (xs, ys) :: (xe, ye) :: _ if (xs == xe) && (ys <= ye) =>
            (ys to ye).map(y => (xs, y))
          case (xs, ys) :: (xe, ye) :: _ if (xs == xe) && (ys > ye) =>
            (ye to ys).map(y => (xs, y))
          case (xs, ys) :: (xe, ye) :: _ if (xs <= xe) && (ys == ye) =>
            (xs to xe).map(x => (x, ys))
          case (xs, ys) :: (xe, ye) :: _ if (xs > xe) && (ys == ye) =>
            (xe to xs).map(x => (x, ys))
        }
      }
      .map { c: (Int, Int) => (c, '#') }
      .toList
  }

  @tailrec
  def dropSand(
      sandCoord: (Int, Int),
      board: Map[(Int, Int), Char],
      maxY: Int,
      hasFloor: Boolean
  ): (Boolean, Map[(Int, Int), Char]) = {
    sandCoord match {
      case (500, 0)
          if board.contains((499, 1)) && board.contains((500, 1)) && board
            .contains((501, 1)) =>
        (false, board.updated((500, 0), '+'))
      case (_, y) if (y > maxY && !hasFloor) => (false, board)
      case (x, y)
          if board.contains((x, y + 1)) && !board.contains((x - 1, y + 1)) =>
        dropSand((x - 1, y + 1), board, maxY, hasFloor)
      case (x, y)
          if board.contains((x, y + 1)) && board.contains(
            (x - 1, y + 1)
          ) && !board.contains((x + 1, y + 1)) =>
        dropSand((x + 1, y + 1), board, maxY, hasFloor)
      case (x, y)
          if board.contains((x, y + 1)) && board.contains(
            (x - 1, y + 1)
          ) && board.contains((x + 1, y + 1)) =>
        (true, board.updated((x, y), '+'))
      case (x, y) if (y == maxY && hasFloor) =>
        (true, board.updated((x, y), '+'))

      case (x, y) => dropSand((x, y + 1), board, maxY, hasFloor)
    }
  }

  def gameOne(board: Map[(Int, Int), Char]): Int = {
    val ys = board.keys.map(_._2)
    val maxY = ys.max + 1

    @tailrec
    def recur(board: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
      val (continue, updatedBoard) = dropSand((500, 0), board, maxY, false)
      if (!continue) {
        board
      } else {
        recur(updatedBoard)
      }
    }

    val finalBoard = recur(board)
    printBoard(finalBoard)
    finalBoard.count { case (_, c) => c == '+' }
  }

  def gameTwo(board: Map[(Int, Int), Char]): Int = {
    val ys = board.keys.map(_._2)
    val maxY = ys.max + 1

    @tailrec
    def recur(board: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
      val (continue, updatedBoard) = dropSand((500, 0), board, maxY, true)
      if (!continue) {
        updatedBoard
      } else {
        recur(updatedBoard)
      }
    }

    val finalBoard = recur(board)
    printBoard(finalBoard)
    finalBoard.count { case (_, c) => c == '+' }
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val data = reader
    .getLines()
    .toSeq

  val board = data
    .foldLeft(List.empty[((Int, Int), Char)]) {
      case (acc, row) => (acc ++ getWalls(row))
    }
    .toMap
  printBoard(board)

  println(s"Problem1: ${gameOne(board)}")
  println(s"Problem1: ${gameTwo(board)}")
}
