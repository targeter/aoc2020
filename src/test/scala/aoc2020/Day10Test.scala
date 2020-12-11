package aoc2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Inspectors, OptionValues}
import shared.ops._

class Day10Test extends AnyFunSuite with Matchers with OptionValues with Inspectors {

  import aoc2020.Day10._

  val example1 = """16
                   |10
                   |15
                   |5
                   |1
                   |11
                   |7
                   |19
                   |6
                   |12
                   |4""".stripMargin.asInts

  val example2 = """28
                   |33
                   |18
                   |42
                   |31
                   |14
                   |46
                   |20
                   |48
                   |47
                   |24
                   |23
                   |49
                   |45
                   |19
                   |38
                   |39
                   |11
                   |1
                   |32
                   |25
                   |35
                   |8
                   |17
                   |7
                   |9
                   |4
                   |2
                   |34
                   |10
                   |3""".stripMargin.asInts

  test("deviceJolts") {
    deviceJolts(example1) must be(22)
    deviceJolts(example2) must be(52)
  }

  test("diffs") {
    diffs(example1).count(_ == 1) must be(7)
    diffs(example1).count(_ == 3) must be(5)
    diffs(example2).count(_ == 1) must be(22)
    diffs(example2).count(_ == 3) must be(10)
  }

  test("step1") {
    step1(example1) must be(35)
    step1(example2) must be(220)
    step1(inputInts) must be(1690)
  }

  test("step2") {
    step2(example1) must be(8)
    step2(example2) must be(19208)
    step2(inputInts) must be(5289227976704L)
  }

}
