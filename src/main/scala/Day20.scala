package org.norrehem.aoc

import scala.io.Source
//   8328
// -24968
//  -6305
//  10831 too low

object Day20 extends App {

  case class Numbers(numbers: Seq[(Int, Long)]) {
    private final val len = numbers.size

    def move(index: Int): Numbers = {
      val splitPoint = numbers.indexWhere(_._1 == index)
      val (_, n) = numbers(splitPoint)
      val (pre, post) = numbers.splitAt(splitPoint)
      val others = post.tail ++ pre
      val nn = Math.floorMod(n, len - 1).toInt
      val (otherPre, otherPost) = others.splitAt(nn)
      val newNumbers = ((index, n) +: otherPost) ++ otherPre
      Numbers(newNumbers)
    }

    def find(n: Int, marker: Long): Long = {
      val markerIndex = numbers.indexWhere(_._2 == marker)
      val wrappedIndex = Math.floorMod(markerIndex + n, len)
      numbers(wrappedIndex)._2
    }
  }

  def decode(numbers: Numbers): Numbers = {
    numbers.numbers.indices.foldLeft(Numbers(numbers.numbers)) {
      case (num, index) =>
        num.move(index)
    }
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val numbers = reader
    .getLines()
    .toSeq
    .map(_.toInt)
  println(s"Number of numbers: ${numbers.size}")
  println(s"Max number = ${numbers.max}, min number = ${numbers.min}")
  val decoded = decode(Numbers(numbers.zipWithIndex.map { case (n, i) =>
    (i, n.toLong)
  }))
  println(decoded.numbers.filter(_._2 == 0))
  val after1000 = decoded.find(1000, 0)
  val after2000 = decoded.find(2000, 0)
  val after3000 = decoded.find(3000, 0)
  println(s"1000 after 0 : $after1000")
  println(s"2000 after 0 : $after2000")
  println(s"3000 after 0 : $after3000")
  println(s"Problem1: ${after1000 + after2000 + after3000}")

  val newNumbers = numbers.map(_ * 811589153L)
  val decoded10 = (1 to 10).foldLeft(Numbers(newNumbers.zipWithIndex.map {
    case (n, i) => (i, n)
  })) { case (acc, _) =>
    decode(acc)
  }
  val after1000_2 = decoded10.find(1000, 0)
  val after2000_2 = decoded10.find(2000, 0)
  val after3000_2 = decoded10.find(3000, 0)
  println(s"1000 after 0 : $after1000_2")
  println(s"2000 after 0 : $after2000_2")
  println(s"3000 after 0 : $after3000_2")
  println(s"Problem1: ${after1000_2 + after2000_2 + after3000_2}")

}
