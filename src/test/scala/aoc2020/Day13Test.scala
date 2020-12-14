package aoc2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Inspectors, OptionValues}

class Day13Test extends AnyFunSuite with Matchers with OptionValues with Inspectors {

  import Day13._

  val ex = """939
             |7,13,x,x,59,x,31,19""".stripMargin.split("\n").toList

  test("step1") {
    step1(ex) must be (295)
  }
  test("step2") {
    step2(List("", "17,x,13,19")).value must be (3417)
    step2(List("", "67,7,59,61")).value must be (754018)
    step2(List("", "67,7,x,59,61")).value must be (1261476)
    step2(List("", "67,x,7,59,61")).value must be (779210)
    step2(List("", "1789,37,47,1889")).value must be (1202161486)
  }

}
