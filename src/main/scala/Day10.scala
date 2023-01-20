package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {
  val ADDX_RE = raw"addx (.*)".r
  val NOOP_RE = raw"noop".r
  case class Computer(
      pc: Int,
      xReg: Int,
      pipeline: Seq[Int],
      program: Seq[String]
  ) {
    private def getPipeline(instruction: String): Seq[Int] = {
      instruction match {
        case ADDX_RE(dx) => Seq(0, 0 + dx.toInt)
        case NOOP_RE()   => Seq(0)
      }
    }
    def step: (Computer, Boolean) = {
      this match {
        case Computer(pc, x, pl, pr) if pl.nonEmpty =>
          (Computer(pc + 1, x + pl.head, pl.tail, pr), true)
        case Computer(pc, x, _, pr) if pr.nonEmpty =>
          Computer(pc, x, getPipeline(pr.head), pr.tail).step
        case cStop => (cStop, false)
      }
    }
    def getCRTChar: Char =
      if (Math.abs((pc - 1) % 40 - xReg) <= 1) '#' else '.'
  }
  val PROBESTEPS = Seq(20, 60, 100, 140, 180, 220)

  @tailrec
  def run(
      computer: Computer,
      probeValues: Seq[(Int, Int)] = Seq.empty,
      crtChars: Seq[Char] = Seq('#')
  ): (Seq[(Int, Int)], Seq[Char]) = {
    computer.step match {
      case (c, true) if PROBESTEPS.contains(c.pc) =>
        run(c, probeValues :+ ((c.pc, c.xReg)), crtChars :+ c.getCRTChar)
      case (c, true) =>
        run(c, probeValues, crtChars :+ c.getCRTChar)
      case (c, false) => (probeValues, crtChars)
    }
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val program = reader
    .getLines()
    .toSeq
  val computer = Computer(1, 1, Seq.empty, program)
  val (finalState, crtChars) = run(computer)
  println(finalState)
  println(s"Problem1: ${finalState.map { case (pc, x) => pc * x }.sum}")
  println()
  println("Problem2")
  crtChars.sliding(40, 40).foreach(row => println(row.mkString("")))
}
