package aoc2020

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day01Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2020.Day01._
  private val exampleList = List(1721, 979, 366, 299, 675, 1456)

  test("findSum") {
    findSum(n = 2, input = exampleList).value must contain allOf (1721, 299)
    findSum(n = 3, input = exampleList).value must contain allOf (979, 366, 675)
  }

  test("findAnswer") {
    findAnswer(n = 2, input = exampleList).value must be(514579)
    findAnswer(n = 3, input = exampleList).value must be(241861950)

    findAnswer(n = 2).value must be(145875)
    findAnswer(n = 3).value must be(69596112)
  }

}
