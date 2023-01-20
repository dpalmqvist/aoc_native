package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

// 87230 too low

object Day22 extends App {
  val OOB = ' '
  val FREE = '.'
  val WALL = '#'
  val BLOCKED = Seq(OOB, WALL)

  def west(coords: (Int, Int)): (Int, Int) = (coords._1 - 1, coords._2)
  def east(coords: (Int, Int)): (Int, Int) = (coords._1 + 1, coords._2)
  def north(coords: (Int, Int)): (Int, Int) = (coords._1, coords._2 - 1)
  def south(coords: (Int, Int)): (Int, Int) = (coords._1, coords._2 + 1)

  case class Board(squares: Map[(Int, Int), Char]) {
    def getStartPosition: (Int, Int) = {
      val minJ = squares.keySet.map(_._2).min
      val topRowCoordinates = squares.keySet.filter(_._2 == minJ).toSeq.sorted
      topRowCoordinates.find(coord => squares(coord) == FREE).get
    }
  }
  case class CoordinateWrapping(
      startCoords: (Int, Int),
      endCoords: (Int, Int),
      outwardsDirection: String
  ) {
    val (startX, startY) = startCoords
    val (endX, endY) = endCoords
    val coordinateRange: Seq[(Int, Int)] =
      if (startY == endY && (startX < endX)) {
        (startX to endX).map(_ -> startY)
      } else if (startY == endY && (startX >= endX)) {
        Range.inclusive(startX, endX, -1).map(_ -> startY)
      } else if (startX == endX && (startY < endY)) {
        (startY to endY).map(y => (startX, y))
      } else {
        Range.inclusive(startY, endY, -1).map(y => (startX, y))
      }

    private def flipDirection: String => String = {
      case "W" => "E"
      case "E" => "W"
      case "S" => "N"
      case "N" => "S"
    }

    def buildWrapMap(
        other: CoordinateWrapping
    ): Map[((Int, Int), String), ((Int, Int), String)] = {
      assert(
        this.coordinateRange.size == other.coordinateRange.size,
        s"The number of coordinates must be the same ${this} to ${other}"
      )
      this.coordinateRange
        .zip(other.coordinateRange)
        .map { case (fromCoord, toCoord) =>
          (fromCoord, outwardsDirection) ->
            (toCoord, flipDirection(other.outwardsDirection))
        }
        .toMap
    }
  }

  final def goForth(
      coords: (Int, Int),
      heading: String,
      steps: Int,
      edgeMappings: Map[((Int, Int), String), ((Int, Int), String)]
  ): ((Int, Int), String) = {
    @tailrec
    def recur(
        coords: (Int, Int),
        heading: String,
        steps: Int
    ): ((Int, Int), String) = {
      if (steps == 0) {
        (coords, heading)
      } else {
        val (newCoords, newHeading) =
          heading match {
            case "N" if (board.squares.get(north(coords)).contains(FREE)) =>
              (north(coords), "N")
            case "S" if (board.squares.get(south(coords)).contains(FREE)) =>
              (south(coords), "S")
            case "E" if (board.squares.get(east(coords)).contains(FREE)) =>
              (east(coords), "E")
            case "W" if (board.squares.get(west(coords)).contains(FREE)) =>
              (west(coords), "W")
            case "N" if (board.squares.get(north(coords)).contains(WALL)) =>
              (coords, "N")
            case "S" if (board.squares.get(south(coords)).contains(WALL)) =>
              (coords, "S")
            case "E" if (board.squares.get(east(coords)).contains(WALL)) =>
              (coords, "E")
            case "W" if (board.squares.get(west(coords)).contains(WALL)) =>
              (coords, "W")
            case _
                if board.squares
                  .get(edgeMappings((coords, heading))._1)
                  .contains(FREE) =>
              edgeMappings((coords, heading))
            case _ => (coords, heading)
          }
        recur(newCoords, newHeading, steps - 1)
      }
    }
    recur(coords, heading, steps)
  }

  trait Instruction {
    val typ: String
  }

  case class Turn(direction: String) extends Instruction {
    override val typ: String = "turn"
  }
  case class Distance(distance: Int) extends Instruction {
    override val typ: String = "dist"
  }

  def readInstructions(rows: Seq[String]): Seq[Instruction] = {
    val NUMBER_RE = raw"(\d+)(.*)".r
    val DIRECTION_RE = raw"([RL])(.*)".r

    @tailrec
    def recur(
        remaining: String,
        instructions: Seq[Instruction]
    ): Seq[Instruction] = {
      remaining match {
        case "" => instructions
        case NUMBER_RE(number, rest) =>
          recur(rest, instructions :+ Distance(number.toInt))
        case DIRECTION_RE(dir, rest) =>
          recur(rest, instructions :+ Turn(dir))
      }
    }
    recur(rows.last, Seq.empty)
  }

  def readBoard(rows: Seq[String]): Board = {
    val boardRows = rows.take(rows.size - 2)
    Board(boardRows.zipWithIndex.foldLeft(Map.empty[(Int, Int), Char]) {
      case (acc, (row, j)) =>
        row.zipWithIndex.foldLeft(acc) { case (acc, (ch, i)) =>
          acc.updated((i + 1, j + 1), ch)
        }
    })
  }

  case class Walker(position: (Int, Int), heading: String) {
    def getPassword: Long = position match {
      case (i, j) =>
        1000 * j + 4 * i + (heading match {
          case "E" => 0
          case "S" => 1
          case "W" => 2
          case "N" => 3
        })
    }

    def followInstruction(
        instruction: Instruction,
        board: Board,
        edgeMappings: Map[((Int, Int), String), ((Int, Int), String)]
    ): Walker = {
      println(s"      -> $position, $heading     : $instruction")
      val (newPosition, newHeading) = instruction match {
        case Distance(distance) =>
          goForth(position, heading, distance, edgeMappings)
        case Turn(direction) =>
          (heading, direction) match {
            case ("E", "R") => (position, "S")
            case ("E", "L") => (position, "N")
            case ("S", "R") => (position, "W")
            case ("S", "L") => (position, "E")
            case ("W", "R") => (position, "N")
            case ("W", "L") => (position, "S")
            case ("N", "R") => (position, "E")
            case ("N", "L") => (position, "W")
          }
      }
      Walker(newPosition, newHeading)
    }
  }

  val reader = Source.fromFile(args(0), "UTF-8")

  val rows = reader
    .getLines()
    .toSeq
  val board = readBoard(rows)
  val coordinateWrappings = Seq(
    (
      CoordinateWrapping((51, 1), (100, 1), "N"), //
      CoordinateWrapping((1, 151), (1, 200), "W") //
    ),
    (
      CoordinateWrapping((101, 1), (150, 1), "W"), //
      CoordinateWrapping((51, 50), (51, 1), "W") //
    ),
    (
      CoordinateWrapping((150, 50), (150, 1), "E"), //
      CoordinateWrapping((100, 101), (100, 150), "E")
    ),
    (
      CoordinateWrapping((101, 50), (150, 50), "S"), //
      CoordinateWrapping((100, 51), (100, 100), "E") //
    ),
    (
      CoordinateWrapping((51, 150), (100, 150), "S"),
      CoordinateWrapping((50, 151), (50, 200), "E") //
    ),
    (
      CoordinateWrapping((1, 101), (50, 101), "N"),
      CoordinateWrapping((51, 51), (51, 100), "W") //
    ),
    (
      CoordinateWrapping((101, 1), (150, 1), "N"),
      CoordinateWrapping((1, 200), (50, 200), "S") //
    ),
    (
      CoordinateWrapping((1, 101), (1, 150), "W"),
      CoordinateWrapping((51, 50), (51, 1), "W") //
    )
  )
  /*Seq(
    (
      CoordinateWrapping((9, 1), (9, 4), "W"),
      CoordinateWrapping((5, 5), (8, 5), "N")
    ),
    (
      CoordinateWrapping((9, 1), (12, 1), "N"),
      CoordinateWrapping((4, 5), (1, 5), "N")
    ),
    (
      CoordinateWrapping((1, 8), (4, 8), "S"),
      CoordinateWrapping((12, 12), (9, 12), "S")
    ),
    (
      CoordinateWrapping((5, 8), (8, 8), "S"),
      CoordinateWrapping((9, 12), (9, 9), "W")
    ),
    (
      CoordinateWrapping((13, 9), (16, 9), "N"),
      CoordinateWrapping((12, 8), (12, 5), "E")
    ),
    (
      CoordinateWrapping((12, 1), (12, 4), "E"),
      CoordinateWrapping((16, 12), (16, 9), "E")
    ),
    (
      CoordinateWrapping((13, 12), (16, 12), "S"),
      CoordinateWrapping((1, 8), (1, 5), "W")
    )
  )*/

  val edgeMappings = coordinateWrappings.flatMap { case (leftWrap, rightWrap) =>
    leftWrap.buildWrapMap(rightWrap) ++ rightWrap.buildWrapMap(leftWrap)
  }.toMap
  val instructions = readInstructions(rows)
  val finalWalker = instructions.foldLeft(Walker(board.getStartPosition, "E")) {
    case (walker, instruction) =>
      walker.followInstruction(instruction, board, edgeMappings)
  }
  println(s"Problem1: $finalWalker -> ${finalWalker.getPassword}")
}
