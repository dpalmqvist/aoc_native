package org.norrehem.aoc

object FileSystem {

  case class File(name: String, size: Int)

  case class Dir(name: String, subDirs: Seq[Dir], files: Seq[File]) {
    def size: Int =
      files.map(_.size).sum + subDirs.map(_.size).sum

    def mkdir(path: Seq[String]): Dir = {
      if (path.size == 1) {
        this.copy(subDirs =
          this.subDirs :+ Dir(path.last, Seq.empty, Seq.empty)
        )
      } else {
        val updated =
          subDirs.find(_.name == path.head).map(_.mkdir(path.tail)).toSeq
        this.copy(subDirs =
          this.subDirs.filterNot(_.name == path.head) ++ updated
        )
      }
    }

    def addFile(path: Seq[String], file: File): Dir = {
      if (path.isEmpty) {
        this.copy(files = files :+ file)
      } else {
        val updated =
          subDirs
            .find(_.name == path.head)
            .map(_.addFile(path.tail, file))
            .toSeq
        this.copy(subDirs =
          this.subDirs.filterNot(_.name == path.head) ++ updated
        )
      }
    }

    def sumSmallerThan(sizeLimit: Int, soFar: Int = 0): Int = {
      soFar +
        subDirs.map(_.size).filter(_ <= sizeLimit).sum +
        subDirs
          .map(_.sumSmallerThan(sizeLimit))
          .sum
    }

    def directorySizes: Seq[(Int, String)] = {
      (size, name) +: subDirs.flatMap(_.directorySizes)
    }
  }

}
