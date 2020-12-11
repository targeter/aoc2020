package aoc2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Inspectors, OptionValues}

class Day07Test extends AnyFunSuite with Matchers with OptionValues with Inspectors {

  import aoc2020.Day07._

  val example1 = """light red bags contain 1 bright white bag, 2 muted yellow bags.
                  |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
                  |bright white bags contain 1 shiny gold bag.
                  |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
                  |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
                  |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
                  |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
                  |faded blue bags contain no other bags.
                  |dotted black bags contain no other bags.""".stripMargin

  val example2 = """shiny gold bags contain 2 dark red bags.
                   |dark red bags contain 2 dark orange bags.
                   |dark orange bags contain 2 dark yellow bags.
                   |dark yellow bags contain 2 dark green bags.
                   |dark green bags contain 2 dark blue bags.
                   |dark blue bags contain 2 dark violet bags.
                   |dark violet bags contain no other bags.""".stripMargin

  val exampleLines = example1.split("\n").toList

  test("parse") {
    val expected = List(
      "light red" -> Map("bright white" -> 1, "muted yellow" -> 2),
      "dark orange" -> Map("bright white" -> 3, "muted yellow" -> 4),
      "bright white" -> Map("shiny gold" -> 1),
      "muted yellow" -> Map("shiny gold" -> 2, "faded blue" -> 9),
      "shiny gold" -> Map("dark olive" -> 1, "vibrant plum" -> 2),
      "dark olive" -> Map("faded blue" -> 3, "dotted black" -> 4),
      "vibrant plum" -> Map("faded blue" -> 5, "dotted black" -> 6),
      "faded blue" -> Map(),
      "dotted black" -> Map()
    )
    val result = parse(example1.split("\n").toList)

    forAll(example1.split("\n").toList.zipWithIndex) {
      case (line, index) =>
        parse(Seq(line)) must be(Map(expected(index)))
    }

    expected must contain theSameElementsAs (result)
  }

  test("canReach") {
    implicit val rules = parse(example1.split("\n").toList)
    canReach("light red", "shiny gold") must be(true)
    canReach("dark orange", "shiny gold") must be(true)
    canReach("bright white", "shiny gold") must be(true)
    canReach("muted yellow", "shiny gold") must be(true)
    canReach("shiny gold", "shiny gold") must be(false)
    canReach("dark olive", "shiny gold") must be(false)
    canReach("vibrant plum", "shiny gold") must be(false)
    canReach("faded blue", "shiny gold") must be(false)
    canReach("dotted black", "shiny gold") must be(false)
  }

  test("countChildren") {
    val rules1 = parse(example1.split("\n").toList)
    val rules2 = parse(example2.split("\n").toList)

    countChildren("shiny gold")(rules1) must be(32)
    countChildren("shiny gold")(rules2) must be(126)
  }

}
