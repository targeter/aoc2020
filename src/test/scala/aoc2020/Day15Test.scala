package aoc2020

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day15Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2020.Day15._

  test("test") {
    step1(List(0,3,6))(2020 - 3).last must be(436)
    step1(List(1,3,2))(2020 - 3).last must be(1)
    step1(List(2,1,3))(2020 - 3).last must be(10)
    step1(List(1,2,3))(2020 - 3).last must be(27)
    step1(List(2,3,1))(2020 - 3).last must be(78)
    step1(List(3,2,1))(2020 - 3).last must be(438)
    step1(List(3,1,2))(2020 - 3).last must be(1836)
  }

  test("test2") {

//    step2(List(0,3,6))(2020 - 3)._3 must be(436)
//    step2(List(1,3,2))(2020 - 3)._3 must be(1)
//    step2(List(2,1,3))(2020 - 3)._3 must be(10)
//    step2(List(1,2,3))(2020 - 3)._3 must be(27)
//    step2(List(2,3,1))(2020 - 3)._3 must be(78)
//    step2(List(3,2,1))(2020 - 3)._3 must be(438)
//    step2(List(3,1,2))(2020 - 3)._3 must be(1836)


  }



}
