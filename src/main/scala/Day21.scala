package org.norrehem.aoc

import scala.annotation.tailrec
import scala.io.Source
// 381590356 too low

object Day21 extends App {
  trait Monkey {
    val name: String
    def leftDep: String
    def rightDep: String
    def myValue(knownValues: Map[String, Long]): Option[Long]
    def extractLeft: Monkey
    def extractRight: Monkey
  }

  case class AddMonkey(
      name: String,
      leftDep: String,
      rightDep: String
  ) extends Monkey {
    override def myValue(knownValues: Map[String, Long]): Option[Long] =
      knownValues
        .get(leftDep)
        .flatMap(left => knownValues.get(rightDep).map(right => left + right))

    override def extractLeft: Monkey = SubMonkey(leftDep, name, rightDep)

    override def extractRight: Monkey = SubMonkey(rightDep, name, leftDep)
  }

  case class SubMonkey(
      name: String,
      leftDep: String,
      rightDep: String
  ) extends Monkey {
    override def myValue(knownValues: Map[String, Long]): Option[Long] =
      knownValues
        .get(leftDep)
        .flatMap(left => knownValues.get(rightDep).map(right => left - right))

    override def extractLeft: Monkey = AddMonkey(leftDep, name, rightDep)

    override def extractRight: Monkey = SubMonkey(rightDep, leftDep, name)
  }

  case class MulMonkey(
      name: String,
      leftDep: String,
      rightDep: String
  ) extends Monkey {
    override def myValue(knownValues: Map[String, Long]): Option[Long] =
      knownValues
        .get(leftDep)
        .flatMap(left => knownValues.get(rightDep).map(right => left * right))

    override def extractLeft: Monkey = DivMonkey(leftDep, name, rightDep)

    override def extractRight: Monkey = DivMonkey(rightDep, name, leftDep)
  }

  case class DivMonkey(
      name: String,
      leftDep: String,
      rightDep: String
  ) extends Monkey {
    override def myValue(knownValues: Map[String, Long]): Option[Long] =
      knownValues
        .get(leftDep)
        .flatMap(left => knownValues.get(rightDep).map(right => left / right))

    override def extractLeft: Monkey = MulMonkey(leftDep, name, rightDep)

    override def extractRight: Monkey = DivMonkey(rightDep, leftDep, name)
  }

  val ADD_RE = raw"(\w+)\: (\w+) \+ (\w+)".r
  val SUB_RE = raw"(\w+)\: (\w+) \- (\w+)".r
  val MUL_RE = raw"(\w+)\: (\w+) \* (\w+)".r
  val DIV_RE = raw"(\w+)\: (\w+) \/ (\w+)".r
  val VAL_RE = raw"(\w+)\: (\d+)".r

  def getMonkeys(rows: Seq[String]): (Map[String, Long], Seq[Monkey]) = {
    @tailrec
    def recur(
        rows: Seq[String],
        knownValues: Map[String, Long],
        monkeys: Seq[Monkey]
    ): (Map[String, Long], Seq[Monkey]) = {
      if (rows.isEmpty) {
        (knownValues, monkeys)
      } else {
        val (newKnownValues, newMonkeys) = {
          rows.head match {
            case VAL_RE(name, value) =>
              (knownValues.updated(name, value.toLong), monkeys)
            case ADD_RE(name, leftDep, rightDep) =>
              (knownValues, monkeys :+ AddMonkey(name, leftDep, rightDep))
            case SUB_RE(name, leftDep, rightDep) =>
              (knownValues, monkeys :+ SubMonkey(name, leftDep, rightDep))
            case MUL_RE(name, leftDep, rightDep) =>
              (knownValues, monkeys :+ MulMonkey(name, leftDep, rightDep))
            case DIV_RE(name, leftDep, rightDep) =>
              (knownValues, monkeys :+ DivMonkey(name, leftDep, rightDep))
          }
        }
        recur(rows.tail, newKnownValues, newMonkeys)
      }
    }
    recur(rows, Map.empty[String, Long], Seq.empty[Monkey])
  }

  @tailrec
  def findAllValues(
      knownValues: Map[String, Long],
      monkeys: Seq[Monkey]
  ): (Map[String, Long], Seq[Monkey]) = {
    if (monkeys.isEmpty) {
      (knownValues, Seq.empty)
    } else {
      val (knownMonkeys, unknownMonkeys) =
        monkeys.partition(_.myValue(knownValues).nonEmpty)
      if (knownMonkeys.isEmpty) {
        val (twoKnownMonkeys, reallyUnknownMonkeys) =
          unknownMonkeys.partition(monkey =>
            knownValues.contains(monkey.name) &&
              (knownValues.contains(monkey.leftDep) || knownValues.contains(
                monkey.rightDep
              ))
          )
        if (twoKnownMonkeys.isEmpty) {
          (knownValues, unknownMonkeys)
        } else {
          val (leftKnownMonkey, rightKnownMonkey) =
            twoKnownMonkeys.partition(monkey =>
              knownValues.contains(monkey.leftDep)
            )
          findAllValues(
            knownValues,
            reallyUnknownMonkeys ++ leftKnownMonkey.map(
              _.extractRight
            ) ++ rightKnownMonkey.map(_.extractLeft)
          )
        }
      } else {
        val newKnownValues = knownValues ++ knownMonkeys
          .map(monkey => (monkey.name, monkey.myValue(knownValues).get))
          .toMap
        findAllValues(newKnownValues, unknownMonkeys)
      }
    }
  }

  val reader = Source.fromFile(args(0), "UTF-8")

  val rows = reader
    .getLines()
    .toSeq
  val (knownValues, monkeys) = getMonkeys(rows)

  val (allKnown, _) = findAllValues(knownValues, monkeys)
  println(s"Problem1: ${allKnown("root")}")

  val rootMonkey = monkeys
    .find(_.name == "root")
  val leftVar = rootMonkey.get.leftDep
  val rightVar = rootMonkey.get.rightDep
  val skippedMonkeys = Seq("root", "humn")
  val otherMonkeys =
    monkeys
      .filterNot(monkey => skippedMonkeys.contains(monkey.name))
  val otherKnown = knownValues.filterNot(_._1 == "humn")
  val (allFoundValues, remainingMonkeys) = findAllValues(
    otherKnown,
    otherMonkeys
  )
  val depValue =
    allFoundValues.get(leftVar).getOrElse(allFoundValues(rightVar))
  val (finalValues, _) = findAllValues(
    allFoundValues ++ Map(leftVar -> depValue, rightVar -> depValue),
    remainingMonkeys
  )
  println(s"Problem2: ${finalValues("humn")}")
}
