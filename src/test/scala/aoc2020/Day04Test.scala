package aoc2020

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day04Test extends AnyFunSuite with Matchers with OptionValues {

  val testInput = "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm"
  import aoc2020.Day04._

  test("parse") {
    isValid(parse(testInput).head.keySet) must be(true)
  }

  test("height") {
    "120cm".matches("(\\d\\d\\d*)(cm|in)") must be(true)
    heightValid("150cm") must be(true)
    heightValid("60in") must be(true)
    heightValid("100in") must be(false)
    heightValid("200cm") must be(false)
    heightValid("120") must be(false)
  }
}
