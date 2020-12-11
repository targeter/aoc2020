package aoc2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Inspectors, OptionValues}

class Day09Test extends AnyFunSuite with Matchers with OptionValues with Inspectors {

  val example = """35
                  |20
                  |15
                  |25
                  |47
                  |40
                  |62
                  |55
                  |65
                  |95
                  |102
                  |117
                  |150
                  |182
                  |127
                  |219
                  |299
                  |277
                  |309
                  |576""".stripMargin.split("\n").map(_.toLong).toList

  import aoc2020.Day09._
  test("isValid") {
    isValid((1L to 25L).toList, 26L) must be(true)
    isValid((1L to 25L).toList, 49L) must be(true)
    isValid((1L to 25L).toList, 100L) must be(false)
    isValid((1L to 25L).toList, 50L) must be(false)

    val list = ((1L to 19L) ++ (21L to 25L) :+ 45L).toList
    isValid(list, 26L) must be(true)
    isValid(list, 64L) must be(true)
    isValid(list, 65L) must be(false)
    isValid(list, 66L) must be(true)
  }

  test("firstInvalid") {
    firstInvalid(example, 5).value must be(127L)
  }

  test("findWeakness") {
    findWeakness(example, 127).value must be(62)
  }

}
