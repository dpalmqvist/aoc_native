package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source
//2898 too low
object Day23 extends App {
  def printElves(elves: Seq[Elf]): Unit = {
    val (is, js) = elves.map(_.position).unzip
    val minI = is.min
    val maxI = is.max
    val minJ = js.min
    val maxJ = js.max
    val positions = elves.map(_.position).toSet
    (minJ to maxJ).foreach(j =>
      println(
        (minI to maxI)
          .map(i => if (positions.contains((i, j))) '#' else '.')
          .mkString("")
      )
    )
  }
  val DIRECTIONS: Seq[String] = Seq("N", "S", "W", "E")
  def getDirection(turn: Int, index: Int): String = DIRECTIONS(
    (turn + index) % 4
  )
  def west(coords: (Int, Int)): (Int, Int) = (coords._1 - 1, coords._2)
  def east(coords: (Int, Int)): (Int, Int) = (coords._1 + 1, coords._2)
  def north(coords: (Int, Int)): (Int, Int) = (coords._1, coords._2 - 1)
  def south(coords: (Int, Int)): (Int, Int) = (coords._1, coords._2 + 1)
  def northWest(coords: (Int, Int)): (Int, Int) = west(north(coords))
  def northEast(coords: (Int, Int)): (Int, Int) = east(north(coords))
  def southWest(coords: (Int, Int)): (Int, Int) = west(south(coords))
  def southEast(coords: (Int, Int)): (Int, Int) = east(south(coords))

  case class Elf(id: Int, position: (Int, Int)) {
    def propose(turn: Int, elfPositions: Set[(Int, Int)]): Option[Elf] = {
      if (!anyTaken(position, elfPositions)) {
        None
      } else {
        (0 until 4).flatMap { index =>
          getDirection(turn, index) match {
            case "N" if !northTaken(position, elfPositions) =>
              Some(this.copy(position = north(position)))
            case "S" if !southTaken(position, elfPositions) =>
              Some(this.copy(position = south(position)))
            case "E" if !eastTaken(position, elfPositions) =>
              Some(this.copy(position = east(position)))
            case "W" if !westTaken(position, elfPositions) =>
              Some(this.copy(position = west(position)))
            case _ => None
          }
        }.headOption
      }

    }
  }

  def anyTaken(coords: (Int, Int), elfPositions: Set[(Int, Int)]): Boolean = {
    val neighborCells = Set(
      northWest(coords),
      north(coords),
      northEast(coords),
      east(coords),
      southEast(coords),
      south(coords),
      southWest(coords),
      west(coords)
    )
    elfPositions.exists(neighborCells.contains)
  }

  def westTaken(coords: (Int, Int), elfPositions: Set[(Int, Int)]): Boolean = {
    val westCoords = Set(northWest(coords), west(coords), southWest(coords))
    elfPositions.exists(westCoords.contains)
  }
  def southTaken(coords: (Int, Int), elfPositions: Set[(Int, Int)]): Boolean = {
    val southCoords = Set(southWest(coords), south(coords), southEast(coords))
    elfPositions.exists(southCoords.contains)
  }
  def eastTaken(coords: (Int, Int), elfPositions: Set[(Int, Int)]): Boolean = {
    val eastCoords = Set(southEast(coords), east(coords), northEast(coords))
    elfPositions.exists(eastCoords.contains)
  }
  def northTaken(coords: (Int, Int), elfPositions: Set[(Int, Int)]): Boolean = {
    val northCoords = Set(northEast(coords), north(coords), northWest(coords))
    elfPositions.exists(northCoords.contains)
  }

  def readElves(rows: Seq[String]): Seq[Elf] = {
    rows.zipWithIndex
      .flatMap { case (row, j) =>
        row.zipWithIndex.collect { case ('#', i) => (i, j) }
      }
      .zipWithIndex
      .map { case (position, id) => Elf(id, position) }
  }

  def updateElves(elves: Seq[Elf], turn: Int): (Seq[Elf], Boolean) = {
    val elfPositions = elves.map(_.position).toSet
    val proposals = elves.map(elf => elf.propose(turn, elfPositions))
    val validProposals =
      proposals
        .foldLeft(Map.empty[(Int, Int), Int].withDefaultValue(0)) {
          case (acc, elfOpt) =>
            elfOpt
              .map(elf => acc.updated(elf.position, acc(elf.position) + 1))
              .getOrElse(acc)
        }
        .filter(_._2 == 1)
        .keySet

    val acceptedMoves = proposals.flatMap(proposal =>
      proposal.filter(elf => validProposals.contains(elf.position))
    )
    val (_, staying) =
      elves.partition(elf => acceptedMoves.exists(_.id == elf.id))
    (acceptedMoves ++ staying, acceptedMoves.isEmpty)
  }

  def problem1(elves: Seq[Elf]): Int = {
    val (is, js) = elves.map(_.position).unzip
    val minI = is.min
    val maxI = is.max
    val minJ = js.min
    val maxJ = js.max
    (maxI + 1 - minI) * (maxJ + 1 - minJ) - elves.size
  }

  var lastTime = System.currentTimeMillis()
  @tailrec
  def findMinimalNoMove(elves: Seq[Elf], turn: Int): (Seq[Elf], Int) = {
    val (updatedElves, stop) = updateElves(elves, turn)
    if (stop) {
      (updatedElves, turn)
    } else {
      print('.')
      if (turn % 25 == 0) {
        println(s" - $turn ${System.currentTimeMillis() - lastTime}")
        lastTime = System.currentTimeMillis()
      }
      findMinimalNoMove(updatedElves, turn + 1)
    }
  }

  val reader = Source.fromFile(args(0), "UTF-8")

  val rows = reader
    .getLines()
    .toSeq

  val elves = readElves(rows)
  val (updatedElves, lastTurn) = findMinimalNoMove(elves, 0)
  println(s"Problem1: ${problem1(updatedElves)}")
  println(s"Problem2: ${lastTurn + 1}")
}
