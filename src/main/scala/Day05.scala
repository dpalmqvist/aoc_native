package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends App {
  val MOVE_RE = raw"move (\d+) from (\d+) to (\d+)".r
  def splitToCrates(crateRow: String): Seq[String] = {
    crateRow
      .sliding(4, 4)
      .map(_.trim.replace("[", "").replace("]", ""))
      .toSeq
  }

  def buildCrates(crateRows: Seq[String]): Map[Int, List[String]] = {
    crateRows.foldLeft(
      Map.empty[Int, List[String]].withDefaultValue(List.empty[String])
    ) { case (intermediate, crateRow) =>
      splitToCrates(crateRow).zipWithIndex.foldLeft(intermediate) {
        case (acc, (crate, _)) if crate.isEmpty => acc
        case (acc, (crate, pile)) =>
          acc.updated(pile + 1, acc(pile + 1) :+ crate)
      }
    }
  }

  def buildMoves(moveRows: Seq[String]): Seq[(Int, Int, Int)] = {
    moveRows.foldLeft(Seq.empty[(Int, Int, Int)]) {
      case (moves, MOVE_RE(numCrates, fromPile, toPile)) =>
        moves :+ (numCrates.toInt, fromPile.toInt, toPile.toInt)
    }
  }

  @tailrec
  def crateMover9000(
      crates: Map[Int, List[String]],
      numCrates: Int,
      fromPile: Int,
      toPile: Int
  ): Map[Int, List[String]] = {
    if (numCrates == 0) {
      crates
    } else {
      val pulledCrate = crates(fromPile).head
      crateMover9000(
        crates
          .updated(fromPile, crates(fromPile).tail)
          .updated(toPile, pulledCrate +: crates(toPile)),
        numCrates - 1,
        fromPile,
        toPile
      )
    }
  }

  def crateMover9001(
      crates: Map[Int, List[String]],
      numCrates: Int,
      fromPile: Int,
      toPile: Int
  ): Map[Int, List[String]] = {
    val pulledCrates = crates(fromPile).take(numCrates)
    crates
      .updated(fromPile, crates(fromPile).drop(numCrates))
      .updated(toPile, pulledCrates ++ crates(toPile))
  }

  private def printResults(
      problemNo: Int,
      crates: Map[Int, List[String]]
  ): Unit = {
    println(
      s"Problem $problemNo: ${crates.keys.toSeq.sorted.map(pile => (crates(pile).head)).mkString("")}"
    )
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val rows = reader
    .getLines()
    .toSeq
  val (crateRows, pileRows, moveRows, _) =
    rows.foldLeft(
      (Seq.empty[String], Seq.empty[String], Seq.empty[String], 0)
    ) {
      case ((crates, piles, moves, state), row) if row.trim.isEmpty =>
        (crates, piles, moves, state + 1)
      case ((crates, _, moves, 0), row) if !row.contains("[") =>
        (crates, Seq(row), moves, 0)
      case ((crates, piles, moves, 0), row) => (crates :+ row, piles, moves, 0)
      case ((crates, piles, moves, 1), row) => (crates, piles, moves :+ row, 1)
    }
  val crates = buildCrates(crateRows)
  val moves = buildMoves(moveRows)
  val finalCrates1 = moves.foldLeft(crates) {
    case (intermediate, (numCrates, fromPile, toPile)) =>
      crateMover9000(
        intermediate,
        numCrates,
        fromPile,
        toPile
      )
  }
  val finalCrates2 = moves.foldLeft(crates) {
    case (intermediate, (numCrates, fromPile, toPile)) =>
      crateMover9001(
        intermediate,
        numCrates,
        fromPile,
        toPile
      )
  }
  printResults(1, finalCrates1)
  printResults(2, finalCrates2)

}
