package org.norrehem.aoc

import org.norrehem.aoc.FileSystem.{Dir, File}

import scala.io.Source

object Day07 extends App {
  val COMMAND_RE = raw"[$$]+ (\w+)\s?([\w./]+)?".r
  val DIR_RE = raw"dir (\w+)".r
  val FILE_RE = raw"(\d+) ([\w.]+)".r
  def readDir(rows: Seq[String]): Dir = {
    val root: Dir = rows
      .foldLeft(
        (Seq.empty[String], Dir("/", Seq.empty[Dir], Seq.empty[File]))
      ) {
        case ((cwd, acc), COMMAND_RE(cmd, arg)) =>
          (cmd, arg) match {
            case ("cd", "..")   => (cwd.init, acc)
            case ("cd", "/")    => (Seq.empty[String], acc)
            case ("cd", subDir) => (cwd :+ subDir, acc)
            case ("ls", _)      => (cwd, acc)
          }
        case ((cwd, acc), DIR_RE(dirName)) => //
          (cwd, acc.mkdir(cwd :+ dirName))
        case ((cwd, acc), FILE_RE(fileSize, fileName)) =>
          (cwd, acc.addFile(cwd, File(fileName, fileSize.toInt)))
      }
      ._2
    root
  }
  val reader = Source.fromFile(args(0), "UTF-8")
  val rows = reader
    .getLines()
    .toSeq
  val root = readDir(rows)
  println(s"Problem 1: ${root.sumSmallerThan(100000)}")
  val totalSize = 70000000
  val desiredFree = 30000000
  val currentUsed = root.size
  val required = desiredFree - (totalSize - currentUsed)
  val minViableSize = root.directorySizes.filter(_._1 >= required).min
  println(s"Problem 2: $minViableSize")

}
