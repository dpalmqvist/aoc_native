package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {
  val DO_NARRATE = false
  def narrate(s: String) = if (DO_NARRATE) println(s)
  val MONKEY_NO_RE = raw"Monkey (\d+):".r
  val WORRY_NO_RE = raw"  Starting items: (.*)".r
  val OPERATION_MUL_RE = raw"  Operation: new = old \* (\d+)".r
  val OPERATION_ADD_RE = raw"  Operation: new = old \+ (\d+)".r
  val OPERATION_SQU_RE = raw"  Operation: new = old \* old".r
  val TEST_RE = raw"  Test: divisible by (\d+)".r
  val TRUE_RE = raw"    If true: throw to monkey (\d+)".r
  val FALSE_RE = raw"    If false: throw to monkey (\d+)".r
  def opMul(n: Int) = { (x: Long) =>
    x * n
  }
  def opAdd(n: Int) = { (x: Long) =>
    x + n
  }
  def opSqu = { (x: Long) =>
    x * x
  }
  def test(divisor: Int) = { (x: Long) =>
    x % divisor == 0
  }

  def makeMonkey(
      definition: Seq[String],
      commonDivisor: Long,
      worryDivisor: Int = 3
  ): Monkey = {
    definition.foldLeft(
      Monkey(
        0,
        0,
        Seq.empty,
        _ => 0,
        _ => false,
        0,
        0,
        commonDivisor,
        worryDivisor
      )
    ) {
      case (mke, MONKEY_NO_RE(no)) => mke.copy(no = no.toInt)
      case (mke, WORRY_NO_RE(wry)) =>
        mke.copy(items = wry.split(",").map(_.trim).map(_.toLong).toSeq)
      case (mke, OPERATION_MUL_RE(n)) =>
        mke.copy(operation = opMul(n.toInt))
      case (mke, OPERATION_ADD_RE(n)) => mke.copy(operation = opAdd(n.toInt))
      case (mke, OPERATION_SQU_RE())  => mke.copy(operation = opSqu)
      case (mke, TEST_RE(divisor))    => mke.copy(test = test(divisor.toInt))
      case (mke, TRUE_RE(trueMonkey)) => mke.copy(trueMokey = trueMonkey.toInt)
      case (mke, FALSE_RE(falseMonkey)) =>
        mke.copy(falseMonkey = falseMonkey.toInt)
    }
  }

  case class Monkey(
      no: Int,
      inspections: Long,
      items: Seq[Long],
      operation: Long => Long,
      test: Long => Boolean,
      trueMokey: Int,
      falseMonkey: Int,
      commonDivisor: Long,
      worryDivisor: Int
  ) {
    def step: Map[Int, Seq[Long]] = {
      items.foldLeft(Map.empty[Int, Seq[Long]].withDefaultValue(Seq.empty)) {
        case (acc, item) =>
          narrate(s"Monkey ${no} inspects and item with worry level $item")
          val worry = (operation(item) / worryDivisor) % commonDivisor
          narrate(s"  After inspection worry = $worry")
          val toMonkey = if (test(worry)) trueMokey else falseMonkey
          narrate(s"  Test is ${test(worry)} so pass to $toMonkey")
          acc.updated(toMonkey, acc(toMonkey) :+ worry)
      }
    }
  }

  @tailrec
  def runMonkeys(n: Int, monkeys: Seq[Monkey]): Seq[Monkey] = {
    if (n == 0) {
      monkeys
    } else {
      val thisMonkey = monkeys.head
      val inspections = thisMonkey.items.size
      val output = thisMonkey.step
      val newN = if (thisMonkey.no == monkeys.size - 1) {
        (n - 1)
      } else n
      runMonkeys(
        newN,
        monkeys.tail.map(mke =>
          mke.copy(items = mke.items ++ output(mke.no))
        ) :+ thisMonkey.copy(
          items = Seq.empty,
          inspections = thisMonkey.inspections + inspections
        )
      )
    }
  }

  val reader = Source.fromFile(args(0), "UTF-8")
  val monkeyDescriptions = reader
    .getLines()
    .toSeq
    .mkString("\n")
    .split("\n\n")
  val commonDivisor = monkeyDescriptions.flatMap {
    _.split("\n").collect { case TEST_RE(fac) =>
      fac.toLong
    }
  }.product
  val problem1: Long = {
    val monkeys = monkeyDescriptions.map { monkeDef =>
      makeMonkey(monkeDef.split("\n"), commonDivisor, 3)
    }
    val monkeys20 = runMonkeys(20, monkeys)
    val topMonkeys = monkeys20.sortBy(mke => -mke.inspections).take(2)
    topMonkeys.map(_.inspections).product
  }
  val problem2: Long = {
    val monkeys = monkeyDescriptions.map { monkeDef =>
      makeMonkey(monkeDef.split("\n"), commonDivisor, 1)
    }
    val monkeys10000 = runMonkeys(10000, monkeys)
    val topMonkeys = monkeys10000.sortBy(mke => -mke.inspections).take(2)
    topMonkeys.map(_.inspections).product
  }
  println(s"Problem1: $problem1")
  println(s"Problem2: $problem2")
}
