package aoc2020

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day18Test extends AnyFunSuite with Matchers with OptionValues {

  import Day18._
  import shared.ops.StringOps

    test("step1") {
//    (Num(1) add Num(2) mul Num(3) add Num(4) mul Num(5) add Num(6)).resolve must be(71)
//
//    val expression2 = Num(1) add (Num(2) mul Num(3)) add (Num(4) mul (Num(5) add Num(6)))
//    expression2.resolve must be(51)

  }

  test("parse") {
    Parser.parse("1 + 2 * 3 + 4 * 5 + 6").get must be(71)
    Parser.parse("1 + (2 * 3) + (4 * (5 + 6))").get must be(51)
  }

  test("step2") {
    Parser2.parse("1 + 2 * 3 + 4 * 5 + 6").get must be(231)

  }

}
