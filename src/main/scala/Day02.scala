package org.norrehem.aoc

import scala.io.Source

object Day02 extends App {
  def toWeapon(code: String) = {
    code match {
      case "A" | "X" => 0
      case "B" | "Y" => 1
      case "C" | "Z" => 2
    }
  }

  def matchScore(myWeapon: Int, otherWeapon: Int): Int = {
    // 0 == Equal
    // 1 == I win
    // 2 == You win
    (3 + myWeapon - otherWeapon) % 3 match {
      case 0 => 3
      case 1 => 6
      case 2 => 0
    }
  }

  def weaponScore(weapon: Int): Int = weapon + 1

  def weaponFromOutcome(otherWeapon: Int, desiredOutcome: String): Int = {
    desiredOutcome match {
      case "X" => (3 + otherWeapon - 1) % 3 // Minus one for loss
      case "Y" => (3 + otherWeapon) % 3 // Zero for draw
      case "Z" => (3 + otherWeapon + 1) % 3 // Plus one for win
    }
  }

  def gameOneScore(myCode: String, otherCode: String): Int = {
    val myWeapon = toWeapon(myCode)
    val otherWeapon = toWeapon(otherCode)
    matchScore(myWeapon, otherWeapon) + weaponScore(myWeapon)
  }

  def gameTwoScore(myCode: String, otherCode: String): Int = {
    val otherWeapon = toWeapon(otherCode)
    val myWeapon = weaponFromOutcome(otherWeapon, myCode)
    matchScore(myWeapon, otherWeapon) + weaponScore(myWeapon)
  }

  val EXTRACT_RE = raw"(\w+) (\w+)".r

  val reader = Source.fromFile(args(0), "UTF-8")
  val (gameOne, gameTwo) = reader
    .getLines()
    .map { case EXTRACT_RE(otherCode, myCode) =>
      (gameOneScore(myCode, otherCode), gameTwoScore(myCode, otherCode))
    }
    .toList
    .unzip
  println(s"Game One score: ${gameOne.sum}")
  println(s"Game Two score: ${gameTwo.sum}")

}
