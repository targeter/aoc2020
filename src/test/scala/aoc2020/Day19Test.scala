package aoc2020

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import shared.ops._

class Day19Test extends AnyFunSuite with Matchers with OptionValues {

  import Day19._



  test("step1") {
  val example = """0: 1 2
                  |1: "a"
                  |2: 1 3 | 3 1
                  |3: "b"""".stripMargin.splitLines
    SingeCharPattern.matches("\"a\"") must be(true)
    DoubleRefPattern.matches("1 2") must be(true)
    DoubleOrPattern.matches("1 2 | 2 1") must be(true)

    implicit val rulesMap = parseRules(example)
    println(rulesMap(0).resolve)
    matchLine("aab") must be(true)
    matchLine("aba") must be(true)
    matchLine("bab") must be(false)

  }


}


