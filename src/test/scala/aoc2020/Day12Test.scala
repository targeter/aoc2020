package aoc2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Inspectors, OptionValues}

class Day12Test extends AnyFunSuite with Matchers with OptionValues with Inspectors {

  import aoc2020.Day12._

  val example = """F10
                  |N3
                  |F7
                  |R90
                  |F11""".stripMargin.split("\n").toList

  test("test") {
    step1(example) must be(Location(Position(17, -8), South, Position(10, 1)))
    dist(Position(0, 0), Position(17, -8)) must be(25)
    dist(start.pos, step1(example).pos) must be(25)
  }

  test("test2") {
    step2(example) must be(Location(Position(214, -72), East, Position(4, -10)))
    dist(start.pos, step2(example).pos) must be(286)
  }

  test("left") {
    North.left(90) must be(West)
    North.left(180) must be(South)
    North.left(270) must be(East)

    West.left(90) must be(South)
    West.left(180) must be(East)
    West.left(270) must be(North)

    South.left(90) must be(East)
    South.left(180) must be(North)
    South.left(270) must be(West)


    East.left(90) must be(North)
    East.left(180) must be(West)
    East.left(270) must be(South)
  }

  test("right") {
    North.right(90) must be(East)
    North.right(180) must be(South)
    North.right(270) must be(West)

    West.right(90) must be(North)
    West.right(180) must be(East)
    West.right(270) must be(South)

    South.right(90) must be(West)
    South.right(180) must be(North)
    South.right(270) must be(East)


    East.right(90) must be(South)
    East.right(180) must be(West)
    East.right(270) must be(North)
  }


}
