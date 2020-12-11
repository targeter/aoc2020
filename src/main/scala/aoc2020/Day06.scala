package aoc2020

import shared._

case object Day06 extends AocTools(day = 6) {

  private def groups(input: String): Array[String] = input.split("\n\n")
  private def groupsLines(input: String): Array[Array[Set[Char]]] = groups(input).map(_.split("\n").map(_.toSet))

  def step1(input: String): Int = groupsLines(input).map(_.flatten.toSet.size).sum

  def step2(input: String): Int =
    groupsLines(input).map { groupLines =>
      val allAnswers = groupLines.flatten.toSet
      groupLines
        .foldLeft(allAnswers)(_ intersect _)
        .size
    }.sum

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputBlob))
    println("Step 2: " + step2(inputBlob))
  }

}
