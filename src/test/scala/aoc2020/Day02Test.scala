package aoc2020

import aoc2020.Day02.{passwordsValid1, passwordsValid2}
import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day02Test extends AnyFunSuite with Matchers with OptionValues {
  private val exampleList = List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")

  test("policy1") {
    assert(passwordsValid1(Seq(exampleList(0))) == 1)
    assert(passwordsValid1(Seq(exampleList(1))) == 0)
    assert(passwordsValid1(Seq(exampleList(2))) == 1)

    assert(passwordsValid1(exampleList) == 2)
    assert(passwordsValid1() == 519)
  }
  test("policy2") {
    assert(passwordsValid2(Seq("1-3 a: abcde")) == 1)
    assert(passwordsValid2(Seq("1-3 b: cdefg")) == 0)
    assert(passwordsValid2(Seq("2-9 c: ccccccccc")) == 0)

    assert(passwordsValid2(exampleList) == 1)
    assert(passwordsValid2() == 708)
  }

}
