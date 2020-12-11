package aoc2020

import shared._

case object Day03 extends AocTools(day = 3) {
  private val TREE = '#'

  private def parseGrid(inputLines: Seq[String]): Seq[Seq[Char]] = inputLines.map(LazyList.continually(_).flatten)

  def count(slopeY: Int, slopeX: Int, input: Seq[String] = inputLines): Long = {
    val grid = parseGrid(input)
    val coords = Iterator.from(0, slopeY) zip Iterator.from(0, slopeX)

    coords
      .take(grid.length / slopeY)
      .map { case (y, x) => grid(y)(x) }
      .count(_ == TREE)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + count(1, 3))

    val slopes = List(
      count(1, 1),
      count(1, 3),
      count(1, 5),
      count(1, 7),
      count(2, 1)
    ).product

    println("Step 2: " + slopes)
  }

}
