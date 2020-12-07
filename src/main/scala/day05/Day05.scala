package day05

import shared._

case object Day05 extends AocTools(day = 5) {

  def parseInt(input: String, zero: String, one: String): Int = Integer.parseInt(input.replaceAll(zero, "0").replaceAll(one, "1"), 2)

  def findRow(pass: String): Int = parseInt(pass.take(7), "F", "B")

  def findCol(pass: String): Int = parseInt(pass.drop(7), "L", "R")

  def seatId(pass: String): Int = (findRow(pass) * 8) + findCol(pass)

  def step1(): Int = inputLines.map(seatId).max

  def step2(): Option[Int] =
    inputLines
      .map(seatId)
      .sorted
      .sliding(2)
      .collectFirst {
        case a :: b :: Nil if a + 1 != b => a + 1
      }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1())
    println("Step 2: " + step2())
  }

}
