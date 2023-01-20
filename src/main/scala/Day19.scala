package org.norrehem.aoc

import scala.collection.mutable
import scala.io.Source

object Day19 extends App {
  val READER_RE =
    raw"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.".r

  case class BluePrint(
      id: Int,
      oreOreCost: Int,
      clayOreCost: Int,
      obsidianOreCost: Int,
      obsidianClayCost: Int,
      geodeOreCost: Int,
      geodeObsidianCost: Int
  ) {
    def oreToGeodeFactor: Float =
      geodeObsidianCost * obsidianClayCost * clayOreCost
  }
  def readBlueprint(blueprint: String): BluePrint = {
    blueprint match {
      case READER_RE(
            id,
            oreOre,
            clayOre,
            obsidianOre,
            obsidianClay,
            geodeOre,
            geodeObsidian
          ) =>
        BluePrint(
          id.toInt,
          oreOre.toInt,
          clayOre.toInt,
          obsidianOre.toInt,
          obsidianClay.toInt,
          geodeOre.toInt,
          geodeObsidian.toInt
        )
    }
  }

  def evaluateBlueprint(bluePrint: BluePrint, time: Int): Int = {
    var bestResult: Int = 0
    val cache: mutable.Map[(Int, Int, Int, Int, Int, Int, Int, Int, Int), Int] =
      mutable.Map.empty
    def recur(
        timeLeft: Int,
        ore: Int,
        clay: Int,
        obsidian: Int,
        geode: Int,
        oreRobots: Int,
        clayRobots: Int,
        obsidianRobots: Int,
        geodeRobots: Int
    ): Int = {
      val state = (
        timeLeft,
        ore,
        clay,
        obsidian,
        geode,
        oreRobots,
        clayRobots,
        obsidianRobots,
        geodeRobots
      )
      if (cache.contains(state)) {
        cache(state)
      } else {
        cache(state) = if (timeLeft == 0) {
          geode
        } else if ( // Assume we build one new geodeRobot per time unit
          (timeLeft * (timeLeft - 1) / 2) + geodeRobots * timeLeft + geode <= bestResult
        ) {
          0
        } else {
          // The possible actions:
          // - Build a geode robot
          val geodeRobotResults = Some(
            ore >= bluePrint.geodeOreCost && obsidian >= bluePrint.geodeObsidianCost
          ).collect { case true =>
            recur(
              timeLeft - 1,
              ore + oreRobots - bluePrint.geodeOreCost,
              clay + clayRobots,
              obsidian + obsidianRobots - bluePrint.geodeObsidianCost,
              geode + geodeRobots,
              oreRobots,
              clayRobots,
              obsidianRobots,
              geodeRobots + 1
            )
          }
          // - Build an obsidian robot
          val obsidianRobotResults = Some(
            ore >= bluePrint.obsidianOreCost && clay >= bluePrint.obsidianClayCost
          ).collect { case true =>
            recur(
              timeLeft - 1,
              ore + oreRobots - bluePrint.obsidianOreCost,
              clay + clayRobots - bluePrint.obsidianClayCost,
              obsidian + obsidianRobots,
              geode + geodeRobots,
              oreRobots,
              clayRobots,
              obsidianRobots + 1,
              geodeRobots
            )
          }
          // - Build a clay robot
          val clayRobotResults = Some(ore >= bluePrint.clayOreCost).collect {
            case true =>
              recur(
                timeLeft - 1,
                ore + oreRobots - bluePrint.clayOreCost,
                clay + clayRobots,
                obsidian + obsidianRobots,
                geode + geodeRobots,
                oreRobots,
                clayRobots + 1,
                obsidianRobots,
                geodeRobots
              )
          }
          // - Build an ore robot
          val oreRobotResults = Some(ore >= bluePrint.oreOreCost).collect {
            case true =>
              recur(
                timeLeft - 1,
                ore + oreRobots - bluePrint.oreOreCost,
                clay + clayRobots,
                obsidian + obsidianRobots,
                geode + geodeRobots,
                oreRobots + 1,
                clayRobots,
                obsidianRobots,
                geodeRobots
              )
          }
          // - Do nothing
          val doNothingResult = recur(
            timeLeft - 1,
            ore + oreRobots,
            clay + clayRobots,
            obsidian + obsidianRobots,
            geode + geodeRobots,
            oreRobots,
            clayRobots,
            obsidianRobots,
            geodeRobots
          )
          val currentResult = (Seq(
            doNothingResult
          ) ++ oreRobotResults ++ clayRobotResults ++ obsidianRobotResults ++ geodeRobotResults).max
          if (currentResult > bestResult) {
            bestResult = currentResult
          }
          currentResult
        }
        cache(state)
      }
    }
    recur(time, 0, 0, 0, 0, 1, 0, 0, 0)
  }
  val bluePrint1 = BluePrint(1, 4, 2, 3, 14, 2, 7)
  val bluePrint2 = BluePrint(2, 2, 3, 3, 8, 3, 12)
  val testBluePrints = Seq(bluePrint1, bluePrint2)
  testBluePrints.foreach { bluePrint =>
    val startTime = System.currentTimeMillis()
    println(
      s"Test ${bluePrint.id} -> ${evaluateBlueprint(bluePrint, 24)} ${(System
        .currentTimeMillis() - startTime) / 1000.0}"
    )
  }
  println("Start running Problem1")
  val reader = Source.fromFile(args(0), "UTF-8")
  val bluePrints = reader
    .getLines()
    .toSeq
    .map(readBlueprint)
  var problem1Answer: Int = 0
  bluePrints
    .foreach { bluePrint =>
      val startTime = System.currentTimeMillis()

      val geodes = evaluateBlueprint(bluePrint, 24)
      val runTime = System.currentTimeMillis() - startTime
      problem1Answer += bluePrint.id * geodes
      println(
        s"Blueprint ${bluePrint.id} -> $geodes (${bluePrint.id * geodes}) in ${runTime / 1000.0} sec"
      )
    }
  println(s"Problem1: $problem1Answer")

  println("Start running Test2")
  testBluePrints.foreach { bluePrint =>
    val startTime = System.currentTimeMillis()
    println(
      s"Test ${bluePrint.id} -> ${evaluateBlueprint(bluePrint, 32)} ${(System
        .currentTimeMillis() - startTime) / 1000.0}"
    )
  }

  println("Start running Problem2")
  var problem2Answer: Int = 1
  bluePrints
    .take(3)
    .foreach { bluePrint =>
      val startTime = System.currentTimeMillis()

      val geodes = evaluateBlueprint(bluePrint, 32)
      val runTime = System.currentTimeMillis() - startTime
      problem2Answer *= geodes
      println(
        s"Blueprint ${bluePrint.id} -> $geodes  in ${runTime / 1000.0} sec"
      )
    }
  println(s"Problem2: $problem2Answer")
}
