package aoc2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Inspectors, OptionValues}

class Day11Test extends AnyFunSuite with Matchers with OptionValues with Inspectors {

  import aoc2020.Day11._

  val example = """L.LL.LL.LL
                   |LLLLLLL.LL
                   |L.L.L..L..
                   |LLLL.LL.LL
                   |L.LL.LL.LL
                   |L.LLLLL.LL
                   |..L.L.....
                   |LLLLLLLLLL
                   |L.LLLLLL.L
                   |L.LLLLL.LL""".stripMargin.split("\n").toList

  val round1 = """#.##.##.##
                 |#######.##
                 |#.#.#..#..
                 |####.##.##
                 |#.##.##.##
                 |#.#####.##
                 |..#.#.....
                 |##########
                 |#.######.#
                 |#.#####.##""".stripMargin.split("\n").toList

  val round2 = """#.LL.L#.##
                |#LLLLLL.L#
                |L.L.L..L..
                |#LLL.LL.L#
                |#.LL.LL.LL
                |#.LLLL#.##
                |..L.L.....
                |#LLLLLLLL#
                |#.LLLLLL.L
                |#.#LLLL.##""".stripMargin.split("\n").toList

  val round12 = """#.##.##.##
                  |#######.##
                  |#.#.#..#..
                  |####.##.##
                  |#.##.##.##
                  |#.#####.##
                  |..#.#.....
                  |##########
                  |#.######.#
                  |#.#####.##""".stripMargin.split("\n").toList

  val round22 = """#.LL.LL.L#
                  |#LLLLLL.LL
                  |L.L.L..L..
                  |LLLL.LL.LL
                  |L.LL.LL.LL
                  |L.LLLLL.LL
                  |..L.L.....
                  |LLLLLLLLL#
                  |#.LLLLLL.L
                  |#.LLLLL.L#""".stripMargin.split("\n").toList

  test("roundTrip") {
    parse(example).printIt must be(example.mkString("\n"))
  }
  test("round") {
    parse(example).next must be(parse(round1))
    parse(round1).next must be(parse(round2))
    finalOccupied(example, _.next).value must be(37)
  }

  test("round2") {
    parse(example).next2 must be(parse(round12))
    parse(round12).next2 must be(parse(round22))
    finalOccupied(example, _.next2).value must be(26)

  }

}
