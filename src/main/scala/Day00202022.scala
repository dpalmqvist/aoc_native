package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day00202022 extends App {
  def getDeck(rows: Seq[String]): Seq[Int] = {
    rows
      .filterNot(row => row.contains("Player") || row.trim.isEmpty)
      .map(_.toInt)
  }

  @tailrec
  def play(deck1: Seq[Int], deck2: Seq[Int]): Int = {
    if (deck1.isEmpty) {
      deck2.reverse.zipWithIndex.map { case (card, index) =>
        card * (index + 1)
      }.sum
    } else if (deck2.isEmpty) {
      deck1.reverse.zipWithIndex.map { case (card, index) =>
        card * (index + 1)
      }.sum

    } else {
      val card1 = deck1.head
      val card2 = deck2.head
      val (deck1updated, deck2updated) = if (card1 > card2) {
        (deck1.tail :+ card1 :+ card2, deck2.tail)
      } else {
        (deck1.tail, deck2.tail :+ card2 :+ card1)
      }
      play(deck1updated, deck2updated)
    }
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val rows = reader
    .getLines()
    .toSeq
  val players =
    rows.foldLeft((Seq.empty[String], Seq.empty[String], false)) {
      case ((p1, p2, false), row) if (row.trim.isEmpty) => (p1, p2, true)
      case ((p1, p2, false), row)                       => (p1 :+ row, p2, false)
      case ((p1, p2, true), row)                        => (p1, p2 :+ row, true)
    }

  val deck1 = getDeck(players._1)
  val deck2 = getDeck(players._2)
  println(s"Game 1: ${play(deck1, deck2)}")
}
