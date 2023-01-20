package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day15 extends App {
  case class Sensor(coords: (Int, Int), minDist: Int) {
    val x: Int = coords._1
    val y: Int = coords._2
    val minY: Int = y - minDist
    val maxY: Int = y + minDist
    val minX: Int = x - minDist
    val maxX: Int = x + minDist
    def getAdjacentCoords: Set[(Int, Int)] = (0 to minDist + 1)
      .flatMap(d =>
        Seq(
          (x + d, minY - 1 + d),
          (x + d, maxY + 1 - d),
          (x - d, minY - 1 + d),
          (x - d, maxY + 1 - d)
        )
      )
      .toSet
    def isAvailable(xTest: Int, yTest: Int): Boolean =
      (xTest > maxX) || (xTest < minX) || (yTest > maxY) || (yTest < minY) || (Math
        .abs(yTest - y) + Math.abs(xTest - x)) > minDist
  }
  case class SensorQueue(sensors: Seq[Sensor] = Seq.empty) {
    def add(sensor: Sensor): SensorQueue = SensorQueue(
      (sensors :+ sensor).sortBy(_.maxY)
    )
    def purge(y: Int): SensorQueue = SensorQueue(sensors.filterNot(_.maxY < y))
  }

  def printCave(
      sensorDists: Map[(Int, Int), Char],
      beacons: Seq[(Int, Int)],
      unavailableCoords: Set[(Int, Int)]
  ): Unit = {
    val cave = (unavailableCoords.map(c => (c, '#')) ++ beacons.map(c =>
      (c, 'B')
    ) ++ sensorDists.map { case (c, _) => (c, 'S') }).toMap

    val xMax = cave.keys.map(_._1).max
    val yMax = cave.keys.map(_._2).max
    (0 to yMax).foreach { y =>
      val row = (0 to xMax).map(x => cave.getOrElse((x, y), '.')).mkString("")
      println(row)
    }
  }

  val ROW_RE =
    raw"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)".r

  def getLocations(
      data: Seq[String]
  ): (Seq[Sensor], Seq[(Int, Int)]) = {
    data.map { case ROW_RE(xss, yss, xbs, ybs) =>
      val (xs, ys, xb, yb) = (xss.toInt, yss.toInt, xbs.toInt, ybs.toInt)
      val manhattanDistance = Math.abs(xb - xs) + Math.abs(yb - ys)
      (Sensor((xs, ys), manhattanDistance), (xb, yb))
    }.unzip
  }

  def getSensorsForRow(
      sensorDist: Seq[((Int, Int), Int)],
      yRow: Int
  ): Seq[((Int, Int), Int)] = {
    sensorDist.filter { case ((xs, ys), dist) =>
      (ys - dist) <= yRow && (ys + dist) >= yRow
    }
  }

  def getUnavailableCoords(
      sensors: Seq[Sensor],
      yRow: Int
  ): Set[Int] = {
    sensors.foldLeft(Set.empty[Int]) { case (acc, Sensor((xs, ys), dist)) =>
      val dx = dist - Math.abs(yRow - ys)
      acc ++ (xs - dx to xs + dx)
    }
  }

  def findAvailableAtY(
      y: Int,
      sensorQueue: SensorQueue,
      sensors: Seq[Sensor],
      xMax: Int
  ): (Set[Int], SensorQueue, Seq[Sensor]) = {
    val (newUnpurgedSensorQueue, newSensors) = {
      sensors.foldLeft((sensorQueue, Seq.empty[Sensor])) {
        case ((queue, ss), sensor) if (sensor.minY <= y) =>
          (queue.add(sensor), ss)
        case ((queue, ss), sensor) if (sensor.minY > y) => (queue, ss :+ sensor)
      }
    }
    val newSensorQueue = newUnpurgedSensorQueue.purge(y)
    (
      (0 to xMax).toSet.diff(getUnavailableCoords(newSensorQueue.sensors, y)),
      newSensorQueue,
      newSensors
    )
  }

  @tailrec
  def findMinY(
      y: Int,
      sensorQueue: SensorQueue,
      unqueu: Seq[Sensor],
      xMax: Int
  ): Set[(Int, Int)] = {
    println(s"y = $y, unqueque size = ${unqueu.size}")
    val (xs, newSensorQueue, newUnqueue) =
      findAvailableAtY(y, sensorQueue, unqueu, xMax)
    if (xs.isEmpty) {
      findMinY(y + 1, newSensorQueue, newUnqueue, xMax)
    } else {
      xs.map(_ -> y)
    }
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val data = reader
    .getLines()
    .toSeq
  val (sensors, beacons) = getLocations(data)
  val row1 = 2000000
  val beaconXs = beacons.filter(_._2 == row1).map(_._1)
  val sensorXs = sensors.filter(_.y == row1).map(_.x)
  val excludedXs = (beaconXs ++ sensorXs).toSet

  val unvailableEmpty = getUnavailableCoords(sensors, row1)
  val unavailableCount = unvailableEmpty.diff(excludedXs).size
  println(s"Problem1: $unavailableCount")
  val xMax = 4000000
  val yMax = 4000000
  var ctr: Int = 0
  val theSquares = sensors.zipWithIndex.foldLeft(Set.empty[(Int, Int)]) {
    case (found, (sensor, idx)) if found.isEmpty =>
      println(idx)
      sensor.getAdjacentCoords
        .filter { case (x, y) =>
          sensors.forall(s =>
            (x >= 0) && (x <= xMax) && (y >= 0) && (y <= yMax) && s.isAvailable(
              x,
              y
            )
          )
        }
    case (found, (sensor, idx)) =>
      println(idx)
      found
  }
  println(theSquares)
  println(s"Problem2: ${theSquares
    .map { case (x, y) => x.toLong * 4000000 + y }}")
}
