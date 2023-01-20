package org.norrehem.aoc

import scala.collection.mutable
import scala.io.Source

object Day16 extends App {

  val VALVE_RE =
    raw"Valve ([A-Z]{2}) has flow rate=(\d+); tunnel[s]? lead[s]? to valve[s]? (.*)".r
  case class Valve(name: String, flowRate: Int)
  case class Room(name: String, followers: Seq[String])
  def dfs(
      openValves: Seq[String],
      closedValves: Set[String],
      timeRemaining: Int,
      flowReleased: Int,
      currentRoom: String
  ): Int = {
    if (timeRemaining == 0 || closedValves.isEmpty) {
      flowReleased + timeRemaining * (openValves
        .map(valves(_).flowRate)
        .sum)
    } else {
      closedValves.map { room =>
        val travelTime = travelCost(currentRoom, room)
        if (travelTime < timeRemaining) {
          dfs(
            openValves :+ room,
            closedValves - room,
            timeRemaining - travelTime - 1,
            flowReleased + (travelTime + 1) * (openValves
              .map(valves(_).flowRate)
              .sum),
            room
          )
        } else {
          flowReleased + timeRemaining * (openValves
            .map(valves(_).flowRate)
            .sum)
        }
      }.max
    }
  }

  def pairs(seq: Set[String]): Set[(String, String)] = {
    seq
      .flatMap(first =>
        seq.filterNot(_ == first).map(second => (first, second))
      )
      .toSet
  }

  val travelCostCache: mutable.Map[(String, String), Int] = mutable.Map.empty
  def travelCost(
      startRoom: String,
      endRoom: String
  ): Int = {
    def recur(path: Seq[String]): Int = {
      path match {
        case current :: _ if current == endRoom => path.size - 1
        case current :: _ =>
          val candidates = rooms(current).followers.filterNot(path.contains)
          if (candidates.isEmpty) {
            Int.MaxValue
          } else {
            candidates.map(cand => recur(cand +: path)).min
          }
      }
    }
    if (travelCostCache.contains((startRoom, endRoom))) {
      travelCostCache((startRoom, endRoom))
    } else {
      travelCostCache((startRoom, endRoom)) = recur(Seq(startRoom))
      travelCostCache((startRoom, endRoom))
    }
  }

  def partition(
      valves: Seq[String],
      combination: Int
  ): (Seq[String], Seq[String]) = {
    val (myValves, eleValves, _) =
      valves.foldLeft((Seq.empty[String], Seq.empty[String], combination)) {
        case ((mine, elephants, comb), valve) if (comb % 2 == 0) =>
          (mine :+ valve, elephants, comb / 2)
        case ((mine, elephants, comb), valve) if (comb % 2 == 1) =>
          (mine, elephants :+ valve, comb / 2)
      }
    (myValves, eleValves)
  }

  // MAIN
  val reader = Source.fromFile(args(0), "UTF-8")
  val data = reader
    .getLines()
    .toSeq
  val startTime = System.currentTimeMillis()

  val (unfilteredValves, roomsSeq) = data.map {
    case VALVE_RE(name, flowRate, followers) =>
      (
        Valve(name, flowRate.toInt),
        Room(name, followers.split(",").map(_.trim))
      )
  }.unzip
  val valves =
    unfilteredValves.filter(_.flowRate > 0).map(v => v.name -> v).toMap
  val valveKeys = valves.keys.toSeq
  val maxFlow = valves.values.map(_.flowRate).sum
  val rooms: Map[String, Room] = roomsSeq.map(r => r.name -> r).toMap
  val result1 = dfs(Seq.empty[String], valves.keySet, 30, 0, "AA")
  val result1Time = System.currentTimeMillis()
  println(s"Number of valves with positive flowrate = ${valves.size}")
  println(
    s"Problem1: $result1, ${result1Time - startTime}"
  )
  println(s"Number of combinations ${1 << (valves.size + 1)}")
  val result2 = (0 until 1 << (valves.size + 1)).map { comb =>
    if (comb % 1000 == 0) {
      println(comb)
    }
    val (mine, elephants) = partition(valveKeys, comb)
    dfs(Seq.empty[String], mine.toSet, 26, 0, "AA") +
      dfs(Seq.empty[String], elephants.toSet, 26, 0, "AA")
  }.max
  println(s"Problem2: $result2, ${System.currentTimeMillis() - result1Time}")
}
