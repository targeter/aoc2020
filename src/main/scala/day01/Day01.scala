package day01

import shared._

case object Day01 extends AocTools(day = 1) {
  def findSum(n: Int, input: Seq[Int], target: Int = 2020): Option[Seq[Int]] = input.sorted.combinations(n).find(_.sum == target)

  def findAnswer(n: Int, input: Seq[Int] = inputInts): Option[Int] = findSum(n, input).map(_.product)

  def step1: Option[Int] = findAnswer(n = 2)

  def step2: Option[Int] = findAnswer(n = 3)

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 145875
    println("Step 2: " + step2) // 69596112
  }
}
