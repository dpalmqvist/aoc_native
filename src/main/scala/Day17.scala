package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day17 extends App {
  def printBoard(board: Set[(Int, Int)], rock: Option[Rock] = None): Unit = {
    val minX = 0
    val maxX = CHAMBER_WIDTH - 1
    val minY = 0
    val maxY = rock
      .map(_.getCoords.map(_._2).max)
      .getOrElse(board.map(_._2).max)
    Range.inclusive(maxY, minY, -1).foreach { y =>
      println(
        (minX to maxX)
          .map(x =>
            if (rock.exists(_.getCoords.contains((x, y)))) {
              '@'
            } else if (board.contains((x, y))) '#'
            else '.'
          )
          .mkString("")
      )
    }
  }

  def rowToInt(row: Set[(Int, Int)]): Int = {
    row.map { case (x, _) => 1 << x }.sum
  }

  val CHAMBER_WIDTH: Int = 7
  trait Rock {
    val position: (Int, Int)
    val pieces: Set[(Int, Int)]
    def x: Int = position._1
    def y: Int = position._2
    def getCoords: Set[(Int, Int)] = pieces.map { case (px, py) =>
      (px + x, py + y)
    }
    def collides(board: Set[(Int, Int)]): Boolean = {
      getCoords.exists { case (x, y) =>
        y == 0 || x == -1 || x == CHAMBER_WIDTH
      } || board
        .intersect(getCoords)
        .nonEmpty
    }

    def down: Rock
    def left: Rock
    def right: Rock
  }

  case class Minus(position: (Int, Int)) extends Rock {
    override val pieces: Set[(Int, Int)] = //
      Set((0, 0), (1, 0), (2, 0), (3, 0))

    override def down: Rock =
      this.copy(position = (position._1, position._2 - 1))

    override def left: Rock =
      this.copy(position = (position._1 - 1, position._2))

    override def right: Rock =
      this.copy(position = (position._1 + 1, position._2))

  }

  case class Plus(position: (Int, Int)) extends Rock {
    override val pieces: Set[(Int, Int)] = //
      Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2))
    override def down: Rock =
      this.copy(position = (position._1, position._2 - 1))

    override def left: Rock =
      this.copy(position = (position._1 - 1, position._2))

    override def right: Rock =
      this.copy(position = (position._1 + 1, position._2))

  }

  case class Angle(position: (Int, Int)) extends Rock {
    override val pieces: Set[(Int, Int)] = //
      Set((0, 0), (1, 0), (2, 0), (2, 1), (2, 2))
    override def down: Rock =
      this.copy(position = (position._1, position._2 - 1))

    override def left: Rock =
      this.copy(position = (position._1 - 1, position._2))

    override def right: Rock =
      this.copy(position = (position._1 + 1, position._2))

  }

  case class Vertical(position: (Int, Int)) extends Rock {
    override val pieces: Set[(Int, Int)] = //
      Set((0, 0), (0, 1), (0, 2), (0, 3))
    override def down: Rock =
      this.copy(position = (position._1, position._2 - 1))

    override def left: Rock =
      this.copy(position = (position._1 - 1, position._2))

    override def right: Rock =
      this.copy(position = (position._1 + 1, position._2))

  }

  case class Ball(position: (Int, Int)) extends Rock {
    override val pieces: Set[(Int, Int)] = //
      Set((0, 0), (1, 0), (0, 1), (1, 1))
    override def down: Rock =
      this.copy(position = (position._1, position._2 - 1))

    override def left: Rock =
      this.copy(position = (position._1 - 1, position._2))

    override def right: Rock =
      this.copy(position = (position._1 + 1, position._2))

  }

  def newRock(turn: Int, maxY: Int): Rock = {
    val coord = (2, maxY + 4)
    (turn % 5) match {
      case 0 => Minus(coord)
      case 1 => Plus(coord)
      case 2 => Angle(coord)
      case 3 => Vertical(coord)
      case 4 => Ball(coord)
    }
  }

  def findRest(
      rock: Rock,
      board: Set[(Int, Int)],
      iteration: Int
  ): (Rock, Int) = {
    @tailrec
    def recur(rock: Rock, iteration: Int): (Rock, Int) = {
      //println(s"Iteration: $iteration")
      //printBoard(board, Some(rock))
      val sideRock = wind(iteration % wind.length) match {
        case '>' if rock.right.collides(board) => rock
        case '>'                               => rock.right
        case '<' if rock.left.collides(board)  => rock
        case '<'                               => rock.left
      }
      if (sideRock.down.collides(board)) {
        (sideRock, iteration + 1)
      } else {
        recur(sideRock.down, iteration + 1)
      }
    }
    recur(rock, iteration)
  }

  def part1(numPieces: Int): Int = {
    val (maxY, discarded, iteration, board) = (0 until numPieces).foldLeft(
      (0 /*maxY*/, 0 /* discarded*/, 0 /*iteration*/, Set.empty[(Int, Int)])
    ) { case ((maxY, discarded, iteration, board), turn) =>
      val (rock, updatedIteration) =
        findRest(newRock(turn, maxY), board, iteration)
      val updatedBoard = (board ++ rock.getCoords)
      val maxYPre = updatedBoard.map(_._2).max
      val topRow = updatedBoard.filter(_._2 == maxYPre)
      /*      if (topRow.size == CHAMBER_WIDTH - 1) {
        println(
          s"${rowToInt(topRow)}, ${maxYPre + discarded}, $iteration, $turn"
        )
      }*/
      val discardRows = if (maxYPre > 50) (maxYPre - 50) else 0
      val droppedBoard = updatedBoard
        .map { case (x, y) => (x, y - discardRows) }
        .filter(_._2 > 0)
      (
        droppedBoard.map(_._2).max,
        discarded + discardRows,
        updatedIteration,
        droppedBoard
      )
    }
    maxY + discarded
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val wind = reader
    .getLines()
    .toSeq
    .head
  println(s"Wind cycle: ${wind.length}")
  println(s"Problem1: ${part1(2022)}")
  println("Start of pattern: 834")
  println("Repeating pattern 1740 rocks, yields 2666 in heigh")
  val numberOfIterations = (1000000000000L - 834L) / 1740L
  val remainder = (1000000000000L - 834L) % 1740L
  println(s"Number of iterations: $numberOfIterations")
  println(s"Remainder: $remainder")
  val baseHeight = part1(834 + remainder.toInt)
  println(s"baseHeight = $baseHeight")
  println(s"Problem2: ${baseHeight + numberOfIterations * 2666}")

  //println(s"Problem2: ${part1(wind.length * 30)}")
}
