package org.norrehem.aoc

import scala.io.Source

object Day09 extends App {
  def printTrajectory(trajectory: Seq[(Int, Int)]): Unit = {
    val trace = trajectory.toSet
    val maxX = trajectory.map(_._1).max
    val minX = trajectory.map(_._1).min
    val maxY = trajectory.map(_._2).max
    val minY = trajectory.map(_._2).min
    (minY to maxY).foreach(y =>
      println(
        (minX to maxX)
          .map(x => if (trace.contains((x, y))) "*" else ".")
          .mkString("")
      )
    )
  }
  val EXTRACT_RE = raw"(\w) (\d+)".r
  def headTrajectory(rows: Seq[String]) = {
    rows
      .foldLeft(Seq((0, 0))) {
        case ((x, y) :: tail, "U") => (x, y + 1) +: (x, y) +: tail
        case ((x, y) :: tail, "R") => (x + 1, y) +: (x, y) +: tail
        case ((x, y) :: tail, "D") => (x, y - 1) +: (x, y) +: tail
        case ((x, y) :: tail, "L") => (x - 1, y) +: (x, y) +: tail
      }
      .reverse
  }

  def adjacent(tailPos: (Int, Int), headPos: (Int, Int)): Boolean = {
    (tailPos, headPos) match {
      case ((tx, ty), (hx, hy)) => (
        (Math.abs(tx - hx) <= 1) && (Math.abs(ty - hy) <= 1)
      )
    }
  }

  def inLine(tailPos: (Int, Int), headPos: (Int, Int)): Boolean = {
    (tailPos, headPos) match {
      case ((tx, ty), (hx, hy)) => (
        (tx == hx) || (ty == hy)
      )
    }
  }

  def tailTrajectory(trajectory: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    trajectory
      .foldLeft(Seq(trajectory.head)) {
        case (tailPos, headPos) if adjacent(tailPos.head, headPos) =>
          tailPos
        case (tailPos, headPos) =>
          (headPos, tailPos.head) match {
            case ((hx, hy), (tx, ty))
                if inLine((tx, ty), (hx, hy)) && adjacent(
                  (tx + 1, ty),
                  (hx, hy)
                ) =>
              (tx + 1, ty) +: tailPos
            case ((hx, hy), (tx, ty))
                if inLine((tx, ty), (hx, hy)) && adjacent(
                  (tx - 1, ty),
                  (hx, hy)
                ) =>
              (tx - 1, ty) +: tailPos
            case ((hx, hy), (tx, ty))
                if inLine((tx, ty), (hx, hy)) && adjacent(
                  (tx, ty + 1),
                  (hx, hy)
                ) =>
              (tx, ty + 1) +: tailPos
            case ((hx, hy), (tx, ty))
                if inLine((tx, ty), (hx, hy)) && adjacent(
                  (tx, ty - 1),
                  (hx, hy)
                ) =>
              (tx, ty - 1) +: tailPos
            case ((hx, hy), (tx, ty)) if adjacent((tx + 1, ty + 1), (hx, hy)) =>
              (tx + 1, ty + 1) +: tailPos
            case ((hx, hy), (tx, ty)) if adjacent((tx + 1, ty - 1), (hx, hy)) =>
              (tx + 1, ty - 1) +: tailPos
            case ((hx, hy), (tx, ty)) if adjacent((tx - 1, ty - 1), (hx, hy)) =>
              (tx - 1, ty - 1) +: tailPos
            case ((hx, hy), (tx, ty)) if adjacent((tx - 1, ty + 1), (hx, hy)) =>
              (tx - 1, ty + 1) +: tailPos
            case (hx, hy) =>
              throw new IllegalStateException(
                s"($hx,$hy) is not adjacent to head of \n$tailPos in \n$trajectory"
              )
          }
      }
      .reverse
  }

  def problem1(headTrajectory: Seq[(Int, Int)]): Int = {
    val traj = tailTrajectory(headTrajectory)
    printTrajectory(traj)
    traj.toSet.size
  }

  def problem2(headTrajectory: Seq[(Int, Int)]): Int = {
    val lastTailTraj = (1 to 9)
      .foldLeft(headTrajectory) { case (trajectory, iteration) =>
        val newTraj = tailTrajectory(trajectory)
        newTraj
      }
    printTrajectory(lastTailTraj)
    lastTailTraj.toSet.size
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val rows = reader
    .getLines()
    .toSeq
    .flatMap(_ match {
      case EXTRACT_RE(direction, steps) => Seq.fill(steps.toInt)(direction)
    })
  val headTraj = headTrajectory(rows)
  println(s"Problem1: ${problem1(headTraj)}")
  println(s"Problem2: ${problem2(headTraj)}")

}
