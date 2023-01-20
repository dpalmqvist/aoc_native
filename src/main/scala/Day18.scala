package org.norrehem.aoc

import scala.annotation.tailrec
import scala.collection.convert.ImplicitConversions.`set AsJavaSet`
import scala.collection.mutable
import scala.io.Source

object Day18 extends App {
  def printLayers(
      droplets: Set[(Int, Int, Int)],
      allExteriorNeighbors: Set[(Int, Int, Int)]
  ): Unit = {
    (minZ - 1 to maxZ + 1).foreach { z =>
      println(s"layer: $z")
      (minY - 1 to maxY + 1).foreach { y =>
        val row = (minX - 1 to maxX + 1).map(x =>
          if (droplets.contains((x, y, z))) '#'
          else if (allExteriorNeighbors.contains((x, y, z))) '@'
          else '.'
        )
        println(row.mkString(""))
      }
    }
  }
  val COORD_RE = raw"(\d+),(\d+),(\d+)".r
  def neighbours(coords: (Int, Int, Int)): Set[(Int, Int, Int)] = {
    val (x, y, z) = coords
    Set(
      (x - 1, y, z),
      (x + 1, y, z),
      (x, y - 1, z),
      (x, y + 1, z),
      (x, y, z - 1),
      (x, y, z + 1)
    )
  }

  def findAllNeighbours(
      coordinates: Set[(Int, Int, Int)]
  ): Set[(Int, Int, Int)] = {
    coordinates
      .foldLeft(Set.empty[(Int, Int, Int)]) { case (acc, coord) =>
        acc ++ neighbours(coord._1, coord._2, coord._3)
      }
      .diff(coordinates)
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val droplets = reader
    .getLines()
    .toSeq
    .map { case COORD_RE(x, y, z) =>
      (x.toInt, y.toInt, z.toInt)
    }
    .toSet

  val minX = droplets.map(_._1).min
  val maxX = droplets.map(_._1).max
  val minY = droplets.map(_._2).min
  val maxY = droplets.map(_._2).max
  val minZ = droplets.map(_._3).min
  val maxZ = droplets.map(_._3).max

  val exteriorCoords = mutable.Set.empty[(Int, Int, Int)]
  exteriorCoords.addAll(
    (minX - 1 to maxX + 1)
      .flatMap(x =>
        (minY - 1 to maxY + 1).flatMap(y =>
          (minZ - 1 to maxZ + 1).map(z => (x, y, z))
        )
      )
      .filter { case (x, y, z) =>
        x == minX - 1 || x == maxX + 1 ||
          y == minY - 1 || y == maxY + 1 ||
          z == minZ - 1 || z == maxZ + 1
      }
      .toSet[(Int, Int, Int)]
  )

  @tailrec
  def findAllExterior(
      frontier: Set[(Int, Int, Int)],
      exterior: Set[(Int, Int, Int)] = Set.empty
  ): Set[(Int, Int, Int)] = {
    if (frontier.isEmpty) {
      exterior
    } else {
      val newFrontier = findAllNeighbours(frontier.toSet).filterNot {
        case (x, y, z) =>
          (x < minX - 1) || (x > maxX + 1) ||
            (y < minY - 1) || (y > maxY + 1) ||
            (z < minZ - 1) || (z > maxZ + 1) ||
            exterior.contains((x, y, z)) ||
            droplets.contains((x, y, z))
      }
      findAllExterior(newFrontier, exterior ++ frontier)

    }
  }

  val exposedSides: Int = droplets.foldLeft(0) { case (acc, coords) =>
    acc + neighbours(coords).diff(droplets).size
  }
  println(s"Problem1: ${exposedSides}")

  val allExteriorNeighbors = findAllExterior(Set((minX, minY, minZ)))

  val exteriorExposedSides: Int = droplets.foldLeft(0) { case (acc, coords) =>
    acc + (neighbours(coords)
      .diff(droplets))
      .intersect(allExteriorNeighbors)
      .size
  }
  printLayers(droplets, allExteriorNeighbors)
  println(s"Problem2: ${exteriorExposedSides}")

}
