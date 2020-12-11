package aoc2020

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day03Test extends AnyFunSuite with Matchers with OptionValues {

  val testInput = """..##.........##.........##.........##.........##.........##.......
                    |#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
                    |.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
                    |..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
                    |.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
                    |..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....
                    |.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
                    |.#........#.#........#.#........#.#........#.#........#.#........#
                    |#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
                    |#...##....##...##....##...##....##...##....##...##....##...##....#
                    |.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#""".stripMargin

  test("count") {
    Day03.count(1, 1, testInput.split("\n").toList) must be(2)
    Day03.count(1, 3, testInput.split("\n").toList) must be(7)
    Day03.count(1, 5, testInput.split("\n").toList) must be(3)
    Day03.count(1, 7, testInput.split("\n").toList) must be(4)
    Day03.count(2, 1, testInput.split("\n").toList) must be(2)

  }
}
