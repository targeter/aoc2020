package aoc2020

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day17Test extends AnyFunSuite with Matchers with OptionValues {

  import Day17._
  import shared.ops.StringOps
  val input =
    """.#.
      |..#
      |###""".stripMargin.splitLines

  test("step1") {
    LazyList.iterate(Cube(parse[ThreeCoordinate](input)))(_.step)(6).active must be(112)
  }

  test("step2") {
    LazyList.iterate(Cube(parse[HyperCoordinate](input)))(_.step)(6).active must be(848)
  }

}
