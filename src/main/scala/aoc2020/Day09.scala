package aoc2020

import shared._

case object Day09 extends AocTools(day = 9) {
  def isValid(inlist: List[Long], target: Long): Boolean = inlist.combinations(2).exists(_.sum == target)

  def firstInvalid(input: List[Long], preambleLength: Int = 25): Option[Long] =
    input
      .sliding(preambleLength + 1, 1)
      .dropWhile(l => isValid(l.take(preambleLength), l.last))
      .nextOption()
      .map(_.last)

  def findWeakness(input: List[Long], invalid: Long): Option[Long] =
    (2 to input.length).view
      .flatMap(input.sliding(_, 1).takeWhile(_.sum <= invalid))
      .find(_.sum == invalid)
      .map(result => result.min + result.max)

  def main(args: Array[String]): Unit = {
    val longs = inputLines.map(_.toLong)
    val invalid = firstInvalid(longs)

    println("Step 1: " + invalid)
    println("Step 2: " + invalid.flatMap(findWeakness(longs, _)))
  }

}
